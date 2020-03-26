// cargo-deps: chrono
extern crate chrono;
use chrono::Local;
use std::env;
use std::error;
use std::fs;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufWriter;
use std::process;

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
    pub a: f64,
    pub alpha_12: f64,
    pub dx: f64,
    pub dt: f64,
    pub dir_name: String,
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

        let dir_name = Local::now()
            .format(&format!(
                "workspace/%Y%m%d-%H%M%S_4-8_burgers_1st_order_Lax-Friedrichs_{}",
                &init_string,
            ))
            .to_string();

        Ok(Config {
            init: init,
            i_max: i_max,
            node: i_max + 1,
            c: 2.0 * PI * ((i_max as f64) + 3.0) / (i_max as f64),
            x_left: x_left,
            x_right: x_right,
            tstop: 2.0,
            a: 1.0,
            alpha_12: 1.0,
            dx: dx,
            dt: 0.2 * dx,
            dir_name: dir_name,
        })
    }
}

fn write_file(
    cnf: &Config,
    n: &i64,
    xs: &[f64],
    us: &[f64],
    ues: &[f64],
    uls: &[f64],
    urs: &[f64],
    fs: &[f64],
) -> Result<(), Box<dyn error::Error>> {
    let file_name: String = format!("{}/{:08}.csv", &cnf.dir_name, &n);
    let file = File::create(file_name).unwrap();
    let mut w = BufWriter::new(file);
    write!(w, "xs,us,ues,uls,urs,fs\n").unwrap();
    for i in 0..cnf.node {
        let s = format!(
            "{},{},{},{},{},{}\n",
            xs[i], us[i], ues[i], urs[i], uls[i], fs[i]
        );
        // unwrapを呼んで書き込みエラーを検知
        write!(w, "{}", s).unwrap();
    }
    // flushを呼ぶことで書き込みエラーを全て拾える
    w.flush().unwrap();
    Ok(())
}

fn exact(cnf: &Config, t: f64, xs: &Vec<f64>, ues: &Vec<f64>) -> Vec<f64> {
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

    let mut n: i64 = 0;
    let mut t: f64 = 0.0;

    // initc
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
    ues = exact(&cnf, t, &xs, &ues);

    let mut uls = vec![0.0; cnf.node];
    let mut urs = vec![0.0; cnf.node];
    let mut fs = vec![0.0; cnf.node];
    write_file(&cnf, &n, &xs, &us, &ues, &uls, &urs, &fs);

    while t <= cnf.tstop {
        n = n + 1;
        t = t + cnf.dt;

        // reconstruction_pc
        for i in 1..(cnf.node - 2) {
            // セル境界 (i+1/2) 左側の値
            uls[i + 1] = us[i];
            // セル境界 (i+1/2) 右側の値
            urs[i + 1] = us[i + 1];
        }

        // riemann_llr
        for i in 2..(cnf.node - 1) {
            fs[i] = 0.5 * (f_flux(uls[i]) + f_flux(urs[i]))
                - 0.5 * f64::abs(cnf.alpha_12) * (urs[i] - uls[i]);
        }

        // update
        for i in 2..(cnf.node - 2) {
            us[i] = us[i] - cnf.dt / cnf.dx * (fs[i + 1] - fs[i]);
        }

        // gc
        us[0] = us[cnf.i_max - 3];
        us[1] = us[cnf.i_max - 2];
        us[cnf.i_max - 1] = us[2];
        us[cnf.i_max] = us[3];

        // exact
        ues = exact(&cnf, t, &xs, &ues);

        write_file(&cnf, &n, &xs, &us, &ues, &uls, &urs, &fs);
    }
    println!("1d_mp4.py を実行して動画ファイルを生成してください.");
}
