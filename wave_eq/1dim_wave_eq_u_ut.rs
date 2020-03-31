// cargo-deps: chrono, gnuplot
// YouTube へのリンク:
// 原点で時間に対する正弦波で波を駆動し, ディリクレ境界条件下で解く.
extern crate chrono;
extern crate gnuplot;
use chrono::Local;
use gnuplot::*;
use std::env;
use std::error;
use std::fs;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufWriter;
use std::process;
use std::process::Command;

const PI: f64 = std::f64::consts::PI;

struct Config {
    pub c: f64,     // 光速 [m/s]
    pub f: f64,     // 原点で駆動させる振動数
    pub dx: f64,    // 空間散文間隔 [m]
    pub dt: f64,    // 時間差分間隔 [s]
    pub nx: usize,  // 計算点数
    pub nt: i64,    // 計算ステップ数
    pub m_pml: i64, // 吸収境界の導電率の上昇曲線の次数(2 - 3次が一般的)
    pub r_pml: f64, // 境界面において実現したい反射係数
    pub n_pml: i64, // PMLの層数、大きいほど計算コストが増えるが、反射率低減可
    pub dir_name: String,
    pub title: String,
    pub specify_png: String,
    pub movie_name: String,
    pub graph_ylim_min: f64,
    pub graph_ylim_max: f64,
}

impl Config {
    pub fn new() -> Result<Config, &'static str> {
        let c = 1.0;
        let f = 5.0;
        let dx = 0.01;
        let dt = dx / c * 0.8;
        let nx = 200;
        let nt = 500;

        let prog_name = "1dim-wave-eq-u-ut";
        let title = Local::now()
            .format(&format!("%Y%m%d-%H%M%S-{}", &prog_name))
            .to_string();
        let dir_name = format!("workspace/{}", &title);

        let specify_png = format!("{}/img.%08d.png", &dir_name);
        let movie_name = format!("{}.tmp.mp4", &title);

        Ok(Config {
            c: c,
            f: f,
            nx: nx,
            nt: nt,
            dx: dx,
            dt: dt,
            m_pml: 3,
            r_pml: 1e-6,
            n_pml: 8,
            dir_name: dir_name,
            title: title,
            specify_png: specify_png,
            movie_name: movie_name,
            graph_ylim_min: -3.0,
            graph_ylim_max: 3.0,
        })
    }
}

struct CalcData {
    pub n: i64,
    pub t: f64,
    pub sigma: Vec<f64>,
    pub x: Vec<f64>,
    pub u: Vec<f64>,  // 元の関数
    pub ut: Vec<f64>, // u の 1 階の時間導関数
}

impl CalcData {
    pub fn new(cnf: &Config) -> CalcData {
        let n: i64 = 0;
        let t: f64 = 0.0;
        let sigma = vec![0.0; cnf.nx];

        let mut x = vec![0.0; cnf.nx];
        x[0] = -(cnf.nx as f64) * cnf.dx / 2.0;
        for i in 1..(cnf.nx) {
            x[i] = x[i - 1] + cnf.dx;
        }

        let u = vec![0.0; cnf.nx];
        let ut = vec![0.0; cnf.nx];
        CalcData {
            n: n,
            t: t,
            sigma: sigma,
            x: x,
            u: u,
            ut: ut,
        }
    }

    pub fn write_all(cnf: &Config, cdata: &CalcData) -> Result<(), Box<dyn error::Error>> {
        CalcData::write_csv(&cnf, &cdata)
            .map_err(|err| println!("{:?}", err))
            .ok();
        CalcData::write_png(&cnf, &cdata)
            .map_err(|err| println!("{:?}", err))
            .ok();
        Ok(())
    }

    pub fn write_csv(cnf: &Config, cdata: &CalcData) -> Result<(), Box<dyn error::Error>> {
        let file_name: String = format!("{}/{:08}.csv", &cnf.dir_name, &cdata.n);
        let file = File::create(file_name).unwrap();
        let mut w = BufWriter::new(file);
        write!(w, "x,u\n").unwrap();
        for i in 0..cnf.nx {
            let s = format!("{},{}\n", &cdata.x[i], &cdata.u[i]);
            // unwrapを呼んで書き込みエラーを検知
            write!(w, "{}", s).unwrap();
        }
        // flushを呼ぶことで書き込みエラーを全て拾える
        w.flush().unwrap();
        Ok(())
    }

    pub fn write_png(cnf: &Config, cdata: &CalcData) -> Result<(), Box<dyn error::Error>> {
        let file_name: String = format!("{}/img.{:08}.png", &cnf.dir_name, &cdata.n);
        let mut fg = Figure::new();
        fg.axes2d()
            .set_title(&cnf.title, &[])
            .set_legend(Graph(1.0), Graph(1.0), &[], &[])
            .set_y_range(Fix(cnf.graph_ylim_min), Fix(cnf.graph_ylim_max))
            .set_x_label("x", &[])
            .set_y_label("u", &[])
            .lines(&cdata.x, &cdata.u, &[Caption("u"), LineWidth(2.5)]);
        fg.save_to_png(file_name, 800, 800)
            .map_err(|err| println!("{:?}", err))
            .ok();

        Ok(())
    }

    pub fn write_mp4(cnf: &Config) -> Result<(), Box<dyn error::Error>> {
        let options = ["-r", "10", "-i", &cnf.specify_png, &cnf.movie_name];

        let run_ffmpeg = Command::new("ffmpeg")
            .args(&options)
            .output()
            .expect("failed to start `ffmpeg`");
        println!("{}", String::from_utf8_lossy(&run_ffmpeg.stdout));

        Ok(())
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let cnf = Config::new().unwrap_or_else(|err| {
        println!("Problem parsing arguments: {}", err);
        process::exit(1);
    });

    fs::create_dir_all(&cnf.dir_name).unwrap_or_else(|why| {
        println!("! {:?}", why.kind());
    });

    // initc
    let mut cdata = CalcData::new(&cnf);
    CalcData::write_csv(&cnf, &cdata)
        .map_err(|err| println!("{:?}", err))
        .ok();
    CalcData::write_png(&cnf, &cdata)
        .map_err(|err| println!("{:?}", err))
        .ok();

    let alpha = cnf.dt / (2.0 * cnf.dx);

    while cdata.n <= cnf.nt {
        println!("Now {}/{} times!", &cdata.n, &cnf.nt);

        cdata.n = cdata.n + 1;
        cdata.t = cdata.t + cnf.dt;

        let mut unew = vec![0.0; cnf.nx];
        let mut utnew = vec![0.0; cnf.nx];

        cdata.u[cnf.nx / 2 - 1] = 0.5 * f64::sin(2.0 * PI * cnf.f * cdata.t);

        for i in 1..(cnf.nx - 2) {
            unew[i] = cdata.u[i] + cnf.dt * cdata.ut[i];
            utnew[i] = cdata.ut[i] + alpha * (cdata.u[i - 1] - 2.0 * cdata.u[i] + cdata.u[i + 1]);
        }

        // bc: ディリクレ境界条件
        unew[0] = 0.0;
        unew[cnf.nx - 1] = 0.0;

        cdata.u = unew.clone();
        cdata.ut = utnew.clone();

        CalcData::write_all(&cnf, &cdata)
            .map_err(|err| println!("{:?}", err))
            .ok();
    }

    CalcData::write_mp4(&cnf)
        .map_err(|err| println!("{:?}", err))
        .ok();
    println!("{} として結果を出力しています.", &cnf.movie_name);
}
