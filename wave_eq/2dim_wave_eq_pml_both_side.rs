// cargo-deps: chrono, ndarray
extern crate chrono;
extern crate ndarray;
use chrono::Local;
use ndarray::prelude::*;
use ndarray::Array2;
use std::error;
use std::fs;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufWriter;
use std::process;

const PI: f64 = std::f64::consts::PI;

struct Config {
    pub c: f64,           // 光速 [m/s]
    pub f: f64,           // 原点で駆動させる振動数
    pub dx: f64,          // x 方向の空間散文間隔 [m]
    pub dy: f64,          // y 方向の空間散文間隔 [m]
    pub dt: f64,          // 時間差分間隔 [s]
    pub nx: usize,        // x 方向の計算点数
    pub ny: usize,        // y 方向の計算点数
    pub nt: i64,          // 計算ステップ数
    pub output_step: i64, // 出力ステップ数
    pub m_pml: f64,       // 吸収境界の導電率の上昇曲線の次数(2 - 3次が一般的)
    pub r_pml: f64,       // 境界面において実現したい反射係数
    pub n_pml: usize,     // PMLの層数、大きいほど計算コストが増えるが、反射率低減可
    pub dir_name: String,
    pub title: String,
    pub specify_png: String,
    pub movie_name: String,
    pub graph_ulim_min: f64,
    pub graph_ulim_max: f64,
}

impl Config {
    pub fn new() -> Result<Config, &'static str> {
        let c = 1.0;
        let f = 5.0;
        let dx = 0.01;
        let dy = 0.01;
        let dt = dx / c * 0.5;
        let nx = 100;
        let ny = 100;
        let nt = 2000;
        let output_step = 5;

        let m_pml = 2.0;
        let r_pml = 0.5;
        let n_pml = 12;

        let prog_name = "2dim-wave-eq-pml-both-side";
        let title = Local::now()
            .format(&format!("%Y%m%d-%H%M%S-{}", &prog_name))
            .to_string();
        let dir_name = format!("workspace/{}", &title);

        let specify_png = format!("{}/img.%08d.png", &dir_name);
        let movie_name = format!("{}.tmp.mp4", &title);

        Ok(Config {
            c: c,
            f: f,
            dx: dx,
            dy: dy,
            dt: dt,
            nx: nx,
            ny: ny,
            nt: nt,
            output_step: output_step,
            m_pml: m_pml,
            r_pml: r_pml,
            n_pml: n_pml,
            dir_name: dir_name,
            title: title,
            specify_png: specify_png,
            movie_name: movie_name,
            graph_ulim_min: -1.5,
            graph_ulim_max: 1.5,
        })
    }
}

struct CalcData {
    pub n: i64,
    pub t: f64,
    pub output_num: i64,
    pub sigma: Array2<f64>,
    pub x: Array1<f64>,
    pub y: Array1<f64>,
    pub u: Array2<f64>,  // 元の関数
    pub w1: Array2<f64>, // PML 用のベクトル値関数
    pub w2: Array2<f64>, // PML 用のベクトル値関数
}

impl CalcData {
    pub fn new(cnf: &Config) -> CalcData {
        let n: i64 = 0;
        let t: f64 = 0.0;
        let output_num: i64 = 0;

        let mut sigma: Array2<f64> = Array::zeros((cnf.nx, cnf.ny));
        // 左側
        for i in 0..(cnf.n_pml) {
            for j in 0..(cnf.nx) {
                sigma[(i, j)] =
                    cnf.r_pml * (((cnf.n_pml - i) as f64) / (cnf.n_pml as f64)).powf(cnf.m_pml);
                sigma[(j, i)] =
                    cnf.r_pml * (((cnf.n_pml - i) as f64) / (cnf.n_pml as f64)).powf(cnf.m_pml);
            }
        }
        // 右側
        for i in (cnf.nx - cnf.n_pml - 1)..(cnf.nx) {
            for j in 0..(cnf.nx) {
                sigma[(i, j)] = cnf.r_pml
                    * (((i - (cnf.nx - cnf.n_pml - 1)) as f64) / (cnf.n_pml as f64))
                        .powf(cnf.m_pml);
                sigma[(j, i)] = cnf.r_pml
                    * (((i - (cnf.nx - cnf.n_pml - 1)) as f64) / (cnf.n_pml as f64))
                        .powf(cnf.m_pml);
            }
        }

        let mut x = Array::zeros(cnf.nx);
        x[0] = 0.0;
        for i in 1..(cnf.nx) {
            x[i] = x[i - 1] + cnf.dx;
        }

        let mut y = Array::zeros(cnf.ny);
        y[0] = 0.0;
        for i in 1..(cnf.ny) {
            y[i] = y[i - 1] + cnf.dy;
        }

        let u = Array::zeros((cnf.nx, cnf.ny));
        let w1 = Array::zeros((cnf.nx, cnf.ny));
        let w2 = Array::zeros((cnf.nx, cnf.ny));
        CalcData {
            n: n,
            t: t,
            output_num: output_num,
            sigma: sigma,
            x: x,
            y: y,
            u: u,
            w1: w1,
            w2: w2,
        }
    }

    pub fn write_all(cnf: &Config, cdata: &CalcData) -> Result<(), Box<dyn error::Error>> {
        CalcData::write_csv(&cnf, &cdata)
            .map_err(|err| println!("{:?}", err))
            .ok();
        Ok(())
    }

    pub fn write_csv(cnf: &Config, cdata: &CalcData) -> Result<(), Box<dyn error::Error>> {
        let file_name: String = format!("{}/{:08}.csv", &cnf.dir_name, &cdata.output_num);
        let file = File::create(file_name).unwrap();
        let mut w = BufWriter::new(file);
        write!(w, "x,y,u\n").unwrap();
        for i in 0..cnf.nx {
            for j in 0..cnf.ny {
                let s = format!("{},{},{}\n", &cdata.x[i], &cdata.y[j], &cdata.u[(i, j)],);
                write!(w, "{}", s).unwrap();
            }
        }
        w.flush().unwrap();
        Ok(())
    }
}

fn main() {
    let cnf = Config::new().unwrap_or_else(|err| {
        println!("Problem parsing arguments: {}", err);
        process::exit(1);
    });

    fs::create_dir_all(&cnf.dir_name).unwrap_or_else(|why| {
        println!("! {:?}", why.kind());
    });

    // initc
    let mut cdata = CalcData::new(&cnf);
    CalcData::write_all(&cnf, &cdata)
        .map_err(|err| println!("{:?}", err))
        .ok();

    let alpha_x = cnf.dt / cnf.dx;
    let alpha_y = cnf.dt / cnf.dy;

    while cdata.n <= cnf.nt {
        if cdata.n % cnf.output_step == 0 {
            println!("Now {}/{} times!", &cdata.n, &cnf.nt);
        }

        cdata.n = cdata.n + 1;
        cdata.t = cdata.t + cnf.dt;

        let mut unew = Array::zeros((cnf.nx, cnf.ny));
        let mut w1new = Array::zeros((cnf.nx, cnf.ny));
        let mut w2new = Array::zeros((cnf.nx, cnf.ny));

        cdata.u[(cnf.nx / 2, cnf.ny / 2)] = 0.5 * f64::sin(2.0 * PI * cnf.f * cdata.t);

        // u と v の計算はわける必要あり.
        for i in 1..(cnf.nx - 1) {
            for j in 1..(cnf.ny - 1) {
                unew[(i, j)] = cdata.u[(i, j)]
                    + alpha_x * (cdata.w1[(i, j)] - cdata.w1[(i - 1, j)])
                    + alpha_y * (cdata.w2[(i, j)] - cdata.w2[(i, j - 1)])
                    - cdata.sigma[(i, j)] * cdata.u[(i, j)];
            }
        }
        for i in 0..(cnf.nx - 2) {
            // for の開始位置に注意
            for j in 0..(cnf.ny - 2) {
                w1new[(i, j)] = cdata.w1[(i, j)] + alpha_x * (unew[(i + 1, j)] - unew[(i, j)])
                    - cdata.sigma[(i, j)] * cdata.w1[(i, j)];
                w2new[(i, j)] = cdata.w2[(i, j)] + alpha_y * (unew[(i, j + 1)] - unew[(i, j)])
                    - cdata.sigma[(i, j)] * cdata.w2[(i, j)];
            }
        }

        cdata.u = unew.clone();
        cdata.w1 = w1new.clone();
        cdata.w2 = w2new.clone();

        if cdata.n % cnf.output_step == 0 {
            CalcData::write_all(&cnf, &cdata)
                .map_err(|err| println!("{:?}", err))
                .ok();
            cdata.output_num += 1;
        }
    }
}
