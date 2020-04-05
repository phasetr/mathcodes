// cargo-deps: chrono, ndarray
extern crate chrono;
extern crate ndarray;
use chrono::Local;
use ndarray::prelude::*;
use std::error;
use std::fs;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufWriter;
use std::process;
use std::process::Command;

struct Config {
    pub c: f64,           // 光速 [m/s]
    pub d: f64,           // 拡散係数
    pub f: f64,           // 原点で駆動させる振動数
    pub dx: f64,          // x 方向の空間散文間隔 [m]
    pub dt: f64,          // 時間差分間隔 [s]
    pub nx: usize,        // x 方向の計算点数
    pub i_center: usize,  // x 方向の中心点
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
        let d = 1.0;
        let f = 5.0;
        let dx = 0.01;
        let dt = dx * 0.001;
        let nx = 100;
        let i_center = nx / 2;
        let output_step = 10;
        let nt = 10000;
        let graph_ulim_min = 0.0;
        let graph_ulim_max = 1.0;

        let m_pml = 2.0;
        let r_pml = 0.5;
        let n_pml = 12;

        let prog_name = "1dim";
        let title = Local::now()
            .format(&format!("%Y%m%d-%H%M%S-{}", &prog_name))
            .to_string();
        let dir_name = format!("workspace/{}", &title);

        let specify_png = format!("{}/img.%08d.png", &dir_name);
        let movie_name = format!("{}.tmp.mp4", &title);

        Ok(Config {
            c: c,
            d: d,
            f: f,
            dx: dx,
            dt: dt,
            nx: nx,
            i_center: i_center,
            nt: nt,
            output_step: output_step,
            m_pml: m_pml,
            r_pml: r_pml,
            n_pml: n_pml,
            dir_name: dir_name,
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
    pub x: Array1<f64>,
    pub u: Array1<f64>, // 元の関数
}

impl CalcData {
    pub fn new(cnf: &Config) -> CalcData {
        let n: i64 = 0;
        let t: f64 = 0.0;
        let output_num: i64 = 0;

        let mut x = Array::zeros(cnf.nx);
        x[0] = 0.0;
        for i in 1..cnf.nx {
            x[i] = x[i - 1] + cnf.dx;
        }

        let mut u = Array::zeros(cnf.nx);
        for i in (cnf.nx/2)..(cnf.nx - 1){
            u[(i)] = 1.0;
        }
        CalcData {
            n: n,
            t: t,
            output_num: output_num,
            x: x,
            u: u,
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
        let file_name: String = format!("{}/{:08}.csv", &cnf.dir_name, &cdata.output_num);
        let file = File::create(file_name).unwrap();
        let mut w = BufWriter::new(file);
        write!(w, "x,u\n").unwrap();
        for i in 0..cnf.nx {
            let s = format!(
                "{},{}\n",
                &cdata.x[i],
                &cdata.u[(i)],
            );
            write!(w, "{}", s).unwrap();
        }
        w.flush().unwrap();
        Ok(())
    }

    pub fn write_png(cnf: &Config, cdata: &CalcData) -> Result<(), Box<dyn error::Error>> {
        let csv_name: String = format!("{}/{:08}.csv", &cnf.dir_name, &cdata.output_num);
        let png_name: String = format!("{}/img.{:08}.png", &cnf.dir_name, &cdata.output_num);
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
            .arg(format!(r#"plot "{}" u 1:2 with lines;"#, &csv_name))
            .output()
            .expect("failed to start `gnuplot`");
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

    fs::create_dir_all(&cnf.dir_name).unwrap_or_else(|why| {
        println!("! {:?}", why.kind());
    });

    // initialize
    let mut cdata = CalcData::new(&cnf);
    CalcData::write_all(&cnf, &cdata)
        .map_err(|err| println!("{:?}", err))
        .ok();

    let alpha_x = cnf.d * cnf.dt / cnf.dx.powf(2.0);

    while cdata.n <= cnf.nt {
        if cdata.n % cnf.output_step == 0 {
            println!("Now {}/{} times!", &cdata.n, &cnf.nt);
        }

        cdata.n = cdata.n + 1;
        cdata.t = cdata.t + cnf.dt;

        let mut unew = Array::zeros(cnf.nx);

        for i in 1..(cnf.nx - 1) {
            unew[(i)] = cdata.u[(i)] + alpha_x * (cdata.u[(i + 1)] - 2.0 * cdata.u[(i)] + cdata.u[(i - 1)]);
        }

        // bc
        unew[(0)] = 0.0;
        unew[(cnf.nx - 1)] = 1.0;

        cdata.u = unew.clone();

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
