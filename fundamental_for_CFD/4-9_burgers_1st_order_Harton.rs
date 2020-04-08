// cargo-deps: chrono, gnuplot
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

pub enum Init {
    Sin,
    Square,
}

impl Init {
    pub fn new(init_str: &str) -> Init {
        match init_str {
            "sin" => Init::Sin,
            "square" => Init::Square,
            _ => Init::Sin, // TODO バリデート: 対象外ならエラーで即終了にしたい
        }
    }
}

struct Config {
    pub init: Init,
    pub i_max: usize,
    pub node: usize,
    pub c: f64,
    pub x_left: f64,
    pub x_right: f64,
    pub tstop: f64,
    pub nstop: i64,
    pub a: f64,
    pub alpha_12: f64,
    pub eps: f64,
    pub dx: f64,
    pub dt: f64,
    pub dir_name: String,
    pub title: String,
    pub specify_png: String,
    pub movie_name: String,
    pub graph_ylim_min: f64,
    pub graph_ylim_max: f64,
}

impl Config {
    pub fn new(args: &[String]) -> Result<Config, &'static str> {
        if args.len() < 2 {
            return Err("正弦波に対しては sin, 矩形波に対しては square を入力してください.");
        }

        let init_string = args[1].clone();
        let init: Init = Init::new(&init_string);
        let i_max = 100;
        let x_left = -1.0;
        let x_right = 1.0;
        let dx = (x_right - x_left) / (i_max as f64);
        let dt = 0.2 * dx;
        let tstop = 2.0;
        let nstop = (tstop / dt) as i64;

        let prog_name = "4-9-burgers-1st-order-Harton";
        let title = Local::now()
            .format(&format!("%Y%m%d-%H%M%S-{}-{}", &prog_name, &init_string))
            .to_string();
        let dir_name = format!("workspace/{}", &title);

        let specify_png = format!("{}/img.%08d.png", &dir_name);
        let movie_name = format!("{}.tmp.mp4", &title);

        Ok(Config {
            init: init,
            i_max: i_max,
            node: i_max + 1,
            c: 2.0 * PI * ((i_max as f64) + 3.0) / (i_max as f64),
            x_left: x_left,
            x_right: x_right,
            tstop: tstop,
            nstop: nstop,
            a: 1.0,
            alpha_12: 1.0,
            eps: 0.25,
            dx: dx,
            dt: dt,
            dir_name: dir_name,
            title: title,
            specify_png: specify_png,
            movie_name: movie_name,
            graph_ylim_min: 0.0,
            graph_ylim_max: 1.1,
        })
    }
}

struct CalcData {
    pub n: i64,
    pub t: f64,
    pub xs: Vec<f64>,
    pub us: Vec<f64>,
    pub ues: Vec<f64>,
    pub uls: Vec<f64>,
    pub urs: Vec<f64>,
    pub fs: Vec<f64>,
}

impl CalcData {
    pub fn new(cnf: &Config) -> CalcData {
        let n: i64 = 0;
        let t: f64 = 0.0;

        let mut xs = vec![0.0; cnf.node];
        xs[0] = cnf.x_left;
        for i in 1..cnf.node {
            xs[i] = xs[i - 1] + cnf.dx;
        }

        let mut us = vec![0.0; cnf.node];
        let mut ues = vec![0.0; cnf.node];
        match &cnf.init {
            Init::Sin => {
                for i in 0..cnf.node {
                    us[i] = 0.5 * (1.1 + f64::sin(cnf.c * (xs[i] - cnf.a * t)));
                    ues[i] = 0.5 * (1.1 + f64::sin(cnf.c * (ues[i] - cnf.a * t)));
                }
            }
            Init::Square => {
                us = vec![0.1; cnf.node];
                ues = vec![0.1; cnf.node];
                for i in ((cnf.node - 1) / 2 - 10)..((cnf.node - 1) / 2 + 10) {
                    us[i] = 1.0;
                    ues[i] = 1.0;
                }
            }
        }

        // exact
        ues = exact(&cnf, &t, &xs, &ues);

        let uls = vec![0.0; cnf.node];
        let urs = vec![0.0; cnf.node];
        let fs = vec![0.0; cnf.node];

        CalcData {
            n,
            t,
            xs,
            us,
            ues,
            uls,
            urs,
            fs,
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
        write!(w, "xs,us,ues,uls,urs,fs\n").unwrap();
        for i in 0..cnf.node {
            let s = format!(
                "{},{},{},{},{},{}\n",
                &cdata.xs[i],
                &cdata.us[i],
                &cdata.ues[i],
                &cdata.urs[i],
                &cdata.uls[i],
                &cdata.fs[i]
            );
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
            .lines(
                &cdata.xs,
                &cdata.us,
                &[Caption("numerical"), LineWidth(2.5)],
            )
            .lines(&cdata.xs, &cdata.ues, &[Caption("exact"), LineWidth(2.5)]);
        fg.save_to_png(file_name, 800, 800);

        Ok(())
    }

    pub fn write_mp4(cnf: &Config, cdata: &CalcData) -> Result<(), Box<dyn error::Error>> {
        let options = ["-r", "10", "-i", &cnf.specify_png, &cnf.movie_name];

        let run_ffmpeg = Command::new("ffmpeg")
            .args(&options)
            .output()
            .expect("failed to start `ffmpeg`");
        println!("{}", String::from_utf8_lossy(&run_ffmpeg.stdout));

        Ok(())
    }
}

fn exact(cnf: &Config, t: &f64, xs: &Vec<f64>, ues: &Vec<f64>) -> Vec<f64> {
    let mut ues_new = ues.clone();
    match &cnf.init {
        Init::Sin => {
            for i in 0..cnf.node {
                let mut f = ues_new[i] - 0.5 * (1.1 + f64::sin(cnf.c * (xs[i] - ues_new[i] * t)));
                let mut df = 1.0 + 0.5 * cnf.c * f64::cos(cnf.c * (xs[i] - ues_new[i] * t)) * t;
                while f64::abs(f) >= 1.0e-4 {
                    ues_new[i] = ues_new[i] - f / df; // ニュートン法の計算式
                    f = ues_new[i] - 0.5 * (1.1 + f64::sin(cnf.c * (xs[i] - ues_new[i] * t)));
                    df = 1.0 + 0.5 * cnf.c * f64::cos(cnf.c * (xs[i] - ues_new[i] * t)) * t;
                }
            }
        }
        Init::Square => {
            let xc = -10.0 * cnf.dx + t;
            let xl = -10.0 * cnf.dx + 0.1 * t;
            let xr = 10.0 * cnf.dx + 0.55 * t;

            for i in 0..cnf.node {
                if xs[i] <= xl {
                    ues_new[i] = 0.1;
                }
                if xs[i] >= xl && xs[i] <= xc {
                    ues_new[i] = 0.9 * (xs[i] - xl) / (xc - xl) + 0.1;
                }
                if xs[i] >= xc && xs[i] <= xr {
                    ues_new[i] = 1.0;
                }
                if xs[i] >= xr {
                    ues_new[i] = 0.1;
                }
            }
        }
    }

    ues_new
}

fn f_flux(x: f64) -> f64 {
    0.5 * x * x
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let cnf = Config::new(&args).unwrap_or_else(|err| {
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

    while cdata.t <= cnf.tstop {
        cdata.n = cdata.n + 1;
        cdata.t = cdata.t + cnf.dt;

        if cdata.n % 10 == 0 {
            println!("Now {}/{} times!", &cdata.n, &cnf.nstop);
        }

        // reconstruction_pc
        for i in 1..(cnf.node - 2) {
            // セル境界 (i+1/2) 左側の値
            cdata.uls[i + 1] = cdata.us[i];
            // セル境界 (i+1/2) 右側の値
            cdata.urs[i + 1] = cdata.us[i + 1];
        }

        // riemann_harten
        for i in 2..(cnf.node - 1) {
            let alpha_12 = 0.5 * (cdata.urs[i] + cdata.uls[i]);
            let mut nu_12 = f64::abs(alpha_12);
            if nu_12 < 2.0 * cnf.eps {
                nu_12 = nu_12 * nu_12 / 4.0 / cnf.eps + cnf.eps;
            }
            cdata.fs[i] = 0.5 * (f_flux(cdata.uls[i]) + f_flux(cdata.urs[i]))
                - 0.5 * nu_12 * (cdata.urs[i] - cdata.uls[i]);
        }

        // update
        for i in 2..(cnf.node - 2) {
            cdata.us[i] = cdata.us[i] - cnf.dt / cnf.dx * (cdata.fs[i + 1] - cdata.fs[i]);
        }

        // gc
        cdata.us[0] = cdata.us[cnf.i_max - 3];
        cdata.us[1] = cdata.us[cnf.i_max - 2];
        cdata.us[cnf.i_max - 1] = cdata.us[2];
        cdata.us[cnf.i_max] = cdata.us[3];

        // exact
        cdata.ues = exact(&cnf, &cdata.t, &cdata.xs, &cdata.ues);

        CalcData::write_all(&cnf, &cdata)
            .map_err(|err| println!("{:?}", err))
            .ok();
    }

    CalcData::write_mp4(&cnf, &cdata)
        .map_err(|err| println!("{:?}", err))
        .ok();
    println!("{} として結果を出力しています.", &cnf.movie_name);
}
