// cargo-deps: chrono, vector3d
extern crate chrono;
extern crate vector3d;
use chrono::Local;
use std::env;
use std::error;
use std::fs;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufWriter;
use std::process;
use vector3d::Vector3d;

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
    pub dx: Vector3d<f64>,
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
        let dx = Vector3d {
            x: (x_right - x_left) / (i_max as f64),
            y: 0.0,
            z: 0.0,
        };

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
            dt: 0.2 * dx.x,
            dir_name: dir_name,
        })
    }
}

struct CalcData {
    pub n: i64,
    pub t: f64,
    pub xs: Vec<Vector3d<f64>>,
    pub us: Vec<Vector3d<f64>>,
    pub ues: Vec<Vector3d<f64>>,
    pub uls: Vec<Vector3d<f64>>,
    pub urs: Vec<Vector3d<f64>>,
    pub fs: Vec<Vector3d<f64>>,
}

fn zeroV3() -> Vector3d<f64> {
    Vector3d {
        x: 0.0,
        y: 0.0,
        z: 0.0,
    }
}

impl CalcData {
    pub fn new(cnf: &Config) -> CalcData {
        let n: i64 = 0;
        let t: f64 = 0.0;

        let mut xs = vec![zeroV3(); cnf.node];
        xs[0].x = cnf.x_left;
        for i in 1..cnf.node {
            xs[i] = xs[i - 1] + cnf.dx;
        }

        let mut us = vec![zeroV3(); cnf.node];
        let mut ues = vec![zeroV3(); cnf.node];
        let opo = Vector3d {
            x: 1.1,
            y: 0.0,
            z: 0.0,
        };
        match &cnf.init {
            Init::Sin => {
                for i in 0..cnf.node {
                    us[i].x = (1.1 + f64::sin(cnf.c * (xs[i].x - cnf.a * t))) * 0.5;
                    ues[i].x = (1.1 + f64::sin((ues[i].x - cnf.a * t) * cnf.c)) * 0.5;
                }
            }
            Init::Square => {
                us = vec![
                    Vector3d {
                        x: 0.1,
                        y: 0.0,
                        z: 0.0
                    };
                    cnf.node
                ];
                ues = vec![
                    Vector3d {
                        x: 0.1,
                        y: 0.0,
                        z: 0.0
                    };
                    cnf.node
                ];
                for i in ((cnf.node - 1) / 2 - 10)..((cnf.node - 1) / 2 + 10) {
                    us[i].x = 1.0;
                    ues[i].x = 1.0;
                }
            }
        }

        // exact
        ues = exact(&cnf, &t, &xs, &ues);

        let uls = vec![zeroV3(); cnf.node];
        let urs = vec![zeroV3(); cnf.node];
        let fs = vec![zeroV3(); cnf.node];

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

    pub fn write_csv(cnf: &Config, cdata: &CalcData) -> Result<(), Box<dyn error::Error>> {
        let file_name: String = format!("{}/data.csv.{}", &cnf.dir_name, &cdata.n);
        let file = File::create(file_name).unwrap();
        let mut w = BufWriter::new(file);
        write!(w, "x,y,z,ux,uy,uz\n").unwrap();
        for i in 0..cnf.node {
            let s = format!(
                "{},{},{},{},{},{}\n",
                &cdata.xs[i].x,
                &cdata.xs[i].y,
                &cdata.xs[i].z,
                &cdata.us[i].x,
                &cdata.us[i].y,
                &cdata.us[i].z
            );
            // unwrapを呼んで書き込みエラーを検知
            write!(w, "{}", s).unwrap();
        }
        // flushを呼ぶことで書き込みエラーを全て拾える
        w.flush().unwrap();
        Ok(())
    }
}

fn exact(
    cnf: &Config,
    t: &f64,
    xs: &Vec<Vector3d<f64>>,
    ues: &Vec<Vector3d<f64>>,
) -> Vec<Vector3d<f64>> {
    let mut ues_new = ues.clone();
    match &cnf.init {
        Init::Sin => {
            for i in 0..cnf.node {
                let mut f =
                    ues_new[i].x - (1.1 + f64::sin(cnf.c * (xs[i].x - ues_new[i].x * t))) * 0.5;
                let mut df = 1.0 + 0.5 * cnf.c * f64::cos(cnf.c * (xs[i].x - ues_new[i].x * t)) * t;
                while f64::abs(f) >= 1.0e-4 {
                    ues_new[i].x = ues_new[i].x - f / df; // ニュートン法の計算式
                    f = ues_new[i].x - 0.5 * (1.1 + f64::sin(cnf.c * (xs[i].x - ues_new[i].x * t)));
                    df = 1.0 + 0.5 * cnf.c * f64::cos(cnf.c * (xs[i].x - ues_new[i].x * t)) * t;
                }
            }
        }
        Init::Square => {
            let xc = -10.0 * cnf.dx.x + t;
            let xl = -10.0 * cnf.dx.x + 0.1 * t;
            let xr = 10.0 * cnf.dx.x + 0.55 * t;

            for i in 0..cnf.node {
                if xs[i].x <= xl {
                    ues_new[i].x = 0.1;
                }
                if xs[i].x >= xl && xs[i].x <= xc {
                    ues_new[i].x = 0.9 * (xs[i].x - xl) / (xc - xl) + 0.1;
                }
                if xs[i].x >= xc && xs[i].x <= xr {
                    ues_new[i].x = 1.0;
                }
                if xs[i].x >= xr {
                    ues_new[i].x = 0.1;
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

    while cdata.t <= cnf.tstop {
        cdata.n = cdata.n + 1;
        cdata.t = cdata.t + cnf.dt;

        // reconstruction_pc
        for i in 1..(cnf.node - 2) {
            // セル境界 (i+1/2) 左側の値
            cdata.uls[i + 1].x = cdata.us[i].x;
            // セル境界 (i+1/2) 右側の値
            cdata.urs[i + 1].x = cdata.us[i + 1].x;
        }

        // riemann_llr
        for i in 2..(cnf.node - 1) {
            cdata.fs[i].x = 0.5 * (f_flux(cdata.uls[i].x) + f_flux(cdata.urs[i].x))
                - 0.5 * f64::abs(cnf.alpha_12) * (cdata.urs[i].x - cdata.uls[i].x);
        }

        // update
        for i in 2..(cnf.node - 2) {
            cdata.us[i].x = cdata.us[i].x - cnf.dt / cnf.dx.x * (cdata.fs[i + 1].x - cdata.fs[i].x);
        }

        // gc
        cdata.us[0].x = cdata.us[cnf.i_max - 3].x;
        cdata.us[1].x = cdata.us[cnf.i_max - 2].x;
        cdata.us[cnf.i_max - 1].x = cdata.us[2].x;
        cdata.us[cnf.i_max].x = cdata.us[3].x;

        // exact
        cdata.ues = exact(&cnf, &cdata.t, &cdata.xs, &cdata.ues);

        CalcData::write_csv(&cnf, &cdata)
            .map_err(|err| println!("{:?}", err))
            .ok();
    }
    println!("1d_mp4.py を実行して動画ファイルを生成してください.");
}
