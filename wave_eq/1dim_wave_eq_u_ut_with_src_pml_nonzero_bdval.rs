// cargo-deps: chrono, ndarray
extern crate chrono;
extern crate ndarray;
use chrono::Local;
use ndarray::prelude::*;
use ndarray::Array1;
use std::error;
use std::fs;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufWriter;
use std::process;
use std::process::Command;

const PI: f64 = std::f64::consts::PI;

struct Config {
    pub c: f64,           // 光速 [m/s]
    pub a: f64,           // 振幅
    pub f: f64,           // 原点で駆動させる振動数
    pub dx: f64,          // 空間散文間隔 [m]
    pub dt: f64,          // 時間差分間隔 [s]
    pub u_bd: f64,        // 境界値
    pub nx: usize,        // 計算点数
    pub i_center: usize,  // x 方向の中心点
    pub nt: i64,          // 計算ステップ数
    pub output_step: i64, // 出力ステップ数
    pub m_pml: f64,       // 吸収境界の導電率の上昇曲線の次数(2 - 3次が一般的)
    pub r_pml: f64,       // 境界面において実現したい反射係数
    pub n_pml: i64,       // PMLの層数、大きいほど計算コストが増えるが、反射率低減可
    pub csv_dir_name: String,
    pub png_dir_name: String,
    pub title: String,
    pub specify_png: String,
    pub movie_name: String,
    pub graph_ulim_min: f64,
    pub graph_ulim_max: f64,
}

impl Config {
    pub fn new() -> Result<Config, &'static str> {
        let c = 1.0;
        let a = 1000.0;
        let f = 5.0;
        let dx = 0.01;
        let dt = dx / c * 0.05;
        let u_bd = 0.2;
        let nx = 200;
        let i_center = nx / 2;
        let nt = 5000;
        let output_step = 10;

        let graph_ulim_min = -3.0;
        let graph_ulim_max = 3.0;

        let m_pml = 2.0;
        let r_pml = 0.5;
        let n_pml = 10;

        let prog_name = "1dim-wave-eq-u-ut-with-src-pml-nonzero-bdval";
        let title = Local::now()
            .format(&format!("%Y%m%d-%H%M%S-{}", &prog_name))
            .to_string();
        let csv_dir_name = format!("workspace/{}_csv", &title);
        let png_dir_name = format!("workspace/{}_png", &title);

        let specify_png = format!("{}/img.%08d.png", &png_dir_name);
        let movie_name = format!("workspace/{}.tmp.mp4", &title);

        Ok(Config {
            c: c,
            a: a,
            f: f,
            nx: nx,
            i_center: i_center,
            nt: nt,
            dx: dx,
            dt: dt,
            u_bd: u_bd,
            output_step: output_step,
            m_pml: m_pml,
            r_pml: r_pml,
            n_pml: n_pml,
            csv_dir_name: csv_dir_name,
            png_dir_name: png_dir_name,
            title: title,
            specify_png: specify_png,
            movie_name: movie_name,
            graph_ulim_min: graph_ulim_min,
            graph_ulim_max: graph_ulim_max,
        })
    }
}

struct CalcData {
    pub n: i64,
    pub t: f64,
    pub output_num: i64,
    pub sigma: Array1<f64>,
    pub x: Array1<f64>,
    pub u_pml: Array1<f64>,  // 元の関数
    pub w_pml: Array1<f64>,  // PML の関数
    pub ut_pml: Array1<f64>, // PML のときの速度の時間導関数
    pub u_ord: Array1<f64>,  // 速度場, 比較実験用
    pub ut_ord: Array1<f64>, // 速度の時間導関数, 比較実験用
}

impl CalcData {
    pub fn new(cnf: &Config) -> CalcData {
        let n: i64 = 0;
        let t: f64 = 0.0;
        let output_num: i64 = 0;

        let mut sigma: Array1<f64> = Array::zeros(cnf.nx);
        // 左側
        for i in 0..(cnf.n_pml as usize) {
            sigma[i] =
                cnf.r_pml * (((cnf.n_pml - i as i64) as f64) / (cnf.n_pml as f64)).powf(cnf.m_pml);
        }
        // 右側
        for i in (cnf.nx - (cnf.n_pml as usize) - 1)..(cnf.nx) {
            sigma[i] = cnf.r_pml
                * (((i - (cnf.nx - (cnf.n_pml as usize) - 1)) as f64) / (cnf.n_pml as f64))
                    .powf(cnf.m_pml);
        }

        let mut x = Array::zeros(cnf.nx);
        x[0] = 0.0;
        for i in 1..(cnf.nx) {
            x[i] = x[i - 1] + cnf.dx;
        }

        let u_pml = Array::from_elem(cnf.nx, cnf.u_bd);
        let w_pml = Array::zeros(cnf.nx);
        let ut_pml = Array::zeros(cnf.nx);
        let u_ord = Array::from_elem(cnf.nx, cnf.u_bd);
        let ut_ord = Array::zeros(cnf.nx);
        CalcData {
            n: n,
            t: t,
            output_num: output_num,
            sigma: sigma,
            x: x,
            u_pml: u_pml,
            w_pml: w_pml,
            ut_pml: ut_pml,
            u_ord: u_ord,
            ut_ord: ut_ord,
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
        let file_name: String = format!("{}/{:08}.csv", &cnf.csv_dir_name, &cdata.output_num);
        let file = File::create(file_name).unwrap();
        let mut w = BufWriter::new(file);
        write!(w, "x,u_pml,u_ord\n").unwrap();
        for i in 0..cnf.nx {
            let s = format!("{},{},{}\n", &cdata.x[i], &cdata.u_pml[i], &cdata.u_ord[i]);
            // unwrapを呼んで書き込みエラーを検知
            write!(w, "{}", s).unwrap();
        }
        // flushを呼ぶことで書き込みエラーを全て拾える
        w.flush().unwrap();
        Ok(())
    }

    pub fn write_png(cnf: &Config, cdata: &CalcData) -> Result<(), Box<dyn error::Error>> {
        let csv_name: String = format!("{}/{:08}.csv", &cnf.csv_dir_name, &cdata.output_num);
        let png_name: String = format!("{}/img.{:08}.png", &cnf.png_dir_name, &cdata.output_num);
        Command::new("gnuplot")
            .arg("-e")
            .arg(r#"set terminal png;"#)
            .arg("-e")
            .arg(r#"set datafile separator ",""#)
            .arg("-e")
            .arg(r#"set ticslevel 0;"#)
            .arg("-e")
            .arg(r#"set dgrid3d 100,100;"#)
            .arg("-e")
            .arg(format!(
                r#"set yrange [{}:{}]"#,
                &cnf.graph_ulim_min, &cnf.graph_ulim_max
            ))
            .arg("-e")
            .arg(format!(r#"set output "{}""#, &png_name))
            .arg("-e")
            .arg(format!(
                r#"plot "{}" using 1:2 title "u-pml" with lines, "{}" using 1:3 title "u-ord" with lines;"#,
                &csv_name,
                &csv_name,
            ))
            .output()
            .expect("failed to start `ffmpeg`");
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
    let cnf = Config::new().unwrap_or_else(|err| {
        println!("Problem parsing arguments: {}", err);
        process::exit(1);
    });

    fs::create_dir_all(&cnf.csv_dir_name).unwrap_or_else(|why| {
        println!("! {:?}", why.kind());
    });
    fs::create_dir_all(&cnf.png_dir_name).unwrap_or_else(|why| {
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

    let alpha_pml = cnf.dt / cnf.dx;
    let alpha_laplacian = cnf.dt / cnf.dx.powf(2.0);

    while cdata.n <= cnf.nt {
        if cdata.n % cnf.output_step == 0 {
            println!("Now {}/{} times!", &cdata.n, &cnf.nt);
        }

        cdata.n = cdata.n + 1;
        cdata.t = cdata.t + cnf.dt;

        // ソースタームつき PML
        let mut u_pmlnew = Array::zeros(cnf.nx);
        let mut w_pmlnew = Array::zeros(cnf.nx);
        let mut ut_pmlnew = Array::zeros(cnf.nx);
        let mut f = Array::zeros(cnf.nx);
        f[cnf.i_center] = cnf.a * f64::sin(2.0 * PI * cnf.f * cdata.t);

        for i in 1..(cnf.nx - 2) {
            if cdata.sigma[i] == 0.0 {
                // 内部領域
                ut_pmlnew[i] = cdata.ut_pml[i]
                    + alpha_laplacian
                        * (cdata.u_pml[i - 1] - 2.0 * cdata.u_pml[i] + cdata.u_pml[i + 1])
                    + f[i] * cnf.dt;
            } else {
                // PML 領域: この領域で v = ut は解かなくていい.
                // ut_pmlnew[i] = cdata.sigma[i] * cdata.ut_pml[i];
            }
        }

        for i in 1..(cnf.nx - 1) {
            if cdata.sigma[i] == 0.0 {
                // 内部領域
                u_pmlnew[i] = cdata.u_pml[i] + cnf.dt * ut_pmlnew[i];
            } else {
                // PML 領域
                u_pmlnew[i] = cdata.u_pml[i] + alpha_pml * (cdata.w_pml[i] - cdata.w_pml[i - 1])
                    - cdata.sigma[i] * (cdata.u_pml[i] - cnf.u_bd);
            }
        }

        for i in 0..(cnf.nx - 2) {
            // for の開始位置に注意
            // 吸収壁の外でも解いておく
            w_pmlnew[i] = cdata.w_pml[i] + alpha_pml * (u_pmlnew[i + 1] - u_pmlnew[i])
                - cdata.sigma[i] * cdata.w_pml[i];
        }

        // bc: ディリクレ境界条件
        u_pmlnew[0] = cnf.u_bd;
        u_pmlnew[cnf.nx - 1] = cnf.u_bd;

        cdata.u_pml = u_pmlnew.clone();
        cdata.w_pml = w_pmlnew.clone();
        cdata.ut_pml = ut_pmlnew.clone();

        //////////////////////////////////////////////////////////////////

        // 比較用: ソースタームつきのふつうの一階化
        let mut u_ordnew = Array::zeros(cnf.nx);
        let mut ut_ordnew = Array::zeros(cnf.nx);

        for i in 1..(cnf.nx - 1) {
            ut_ordnew[i] = cdata.ut_ord[i]
                + alpha_laplacian
                    * (cdata.u_ord[i - 1] - 2.0 * cdata.u_ord[i] + cdata.u_ord[i + 1])
                + f[i] * cnf.dt;
            u_ordnew[i] = cdata.u_ord[i] + cnf.dt * ut_ordnew[i];
        }

        // bc: ディリクレ境界条件
        u_ordnew[0] = cnf.u_bd;
        u_ordnew[cnf.nx - 1] = cnf.u_bd;

        cdata.u_ord = u_ordnew.clone();
        cdata.ut_ord = ut_ordnew.clone();

        if cdata.n % cnf.output_step == 0 {
            CalcData::write_all(&cnf, &cdata)
                .map_err(|err| println!("{:?}", err))
                .ok();
            cdata.output_num += 1;
        }
    }

    CalcData::write_mp4(&cnf)
        .map_err(|err| println!("{:?}", err))
        .ok();
    println!("{} として結果を出力しています.", &cnf.movie_name);
}
