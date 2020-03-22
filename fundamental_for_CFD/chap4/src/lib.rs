extern crate yaml_rust;
use std::error;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufWriter;
use yaml_rust::YamlLoader;
const PI: f64 = std::f64::consts::PI;

pub enum Init {
    Sin,
    Square,
}

pub enum Reconstruction {
    PC,
    LaxWendorff,
    BeamWarming,
    Fromm,
    TVD,
}

pub enum Limiter {
    Minmod,
    Superbee,
    VanLeer,
    VanAlbada,
}

pub enum Riemann {
    Roe,
}

pub struct Config {
    pub init: Init,
    pub init_string: String,
    pub reconstruction: Reconstruction,
    pub reconstruction_string: String,
    pub limiter: Limiter,
    pub limiter_string: String,
    pub riemann: Riemann,
    pub riemann_string: String,
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
}

impl Config {
    pub fn new(args: &[String]) -> Result<Config, &'static str> {
        if args.len() < 2 {
            return Err("設定ファイルを指定してください.");
        }

        // yaml 読み込み
        let filename: String = args[1].clone().parse().unwrap();
        let mut file = File::open(filename).expect("ファイルが開けません.");
        let mut contents = String::new();
        file.read_to_string(&mut contents)
            .expect("ファイルが読めません.");
        let yaml_content = YamlLoader::load_from_str(&contents).unwrap();
        let cnf_yaml = &yaml_content[0];

        // init の置換処理
        let init_yaml: String = cnf_yaml["init"].as_str().unwrap().to_string();
        let init: Init = if init_yaml == "sin" {
            Init::Sin
        } else if init_yaml == "square" {
            Init::Square
        } else {
            // TODO バリデート: 対象外ならエラーで即終了にしたい
            Init::Sin
        };

        let init_string = match init {
            Init::Sin => "smooth",
            _ => "square",
        }
        .to_string();

        // reconstruction の置換処理
        let reconstruction_yaml: String = cnf_yaml["reconstruction"].as_str().unwrap().to_string();
        let reconstruction: Reconstruction = if reconstruction_yaml == "PC" {
            Reconstruction::PC
        } else if reconstruction_yaml == "LaxWendorff" {
            Reconstruction::LaxWendorff
        } else if reconstruction_yaml == "BeamWarming" {
            Reconstruction::BeamWarming
        } else if reconstruction_yaml == "Fromm" {
            Reconstruction::Fromm
        } else if reconstruction_yaml == "TVD" {
            Reconstruction::TVD
        } else {
            // TODO バリデート: 対象外ならエラーで即終了にしたい
            Reconstruction::PC
        };
        let reconstruction_string = match reconstruction {
            Reconstruction::PC => "PC",
            Reconstruction::LaxWendorff => "Lax-Wendorff",
            Reconstruction::BeamWarming => "Beam-Warming",
            Reconstruction::Fromm => "Fromm",
            Reconstruction::TVD => "TVD",
        }
        .to_string();

        // limiter の置換処理
        let limiter_yaml: String = cnf_yaml["limiter"].as_str().unwrap().to_string();
        let limiter: Limiter = match limiter_yaml.as_str() {
            "minmod" => Limiter::Minmod,
            "superbee" => Limiter::Superbee,
            "vanLeer" => Limiter::VanLeer,
            "vanAlbada" => Limiter::VanAlbada,
            _ => Limiter::Minmod,
        };
        let limiter_string: String = match limiter {
            Limiter::Minmod => "minmod",
            Limiter::Superbee => "superbee",
            Limiter::VanLeer => "vanLeer",
            Limiter::VanAlbada => "VanAlbada",
        }
        .to_string();

        // riemann の置換処理
        let riemann_yaml: String = cnf_yaml["riemann"].as_str().unwrap().to_string();
        let riemann: Riemann = if riemann_yaml == "Row" {
            Riemann::Roe
        } else {
            // TODO バリデート: 対象外ならエラーで即終了にしたい
            Riemann::Roe
        };
        let riemann_string: String = match riemann {
            Riemann::Roe => "Roe",
        }
        .to_string();

        // Config に設定する値に変換
        let i_max: usize = cnf_yaml["i_max"].as_i64().unwrap() as usize;
        let x_left: f64 = cnf_yaml["x_left"].as_f64().unwrap() as f64;
        let x_right: f64 = cnf_yaml["x_right"].as_f64().unwrap() as f64;
        let tstop: f64 = cnf_yaml["tstop"].as_f64().unwrap() as f64;
        let a: f64 = cnf_yaml["a"].as_f64().unwrap() as f64;
        let alpha_12: f64 = cnf_yaml["alpha_12"].as_f64().unwrap() as f64;
        let dx: f64 = (x_right - x_left) / (i_max as f64);
        let dt: f64 = 0.2 * dx;

        Ok(Config {
            init: init,
            init_string: init_string,
            reconstruction: reconstruction,
            reconstruction_string: reconstruction_string,
            limiter: limiter,
            limiter_string: limiter_string,
            riemann: riemann,
            riemann_string: riemann_string,
            i_max: i_max,
            node: i_max + 1,
            c: 2.0 * PI * ((i_max as f64) + 3.0) / (i_max as f64),
            x_left: x_left,
            x_right: x_right,
            tstop: tstop,
            a: a,
            alpha_12: alpha_12,
            dx: dx,
            dt: dt,
        })
    }
}

pub fn init(cnf: &Config) -> (Vec<f64>, Vec<f64>) {
    let mut xs = vec![0.0; cnf.node];
    xs[0] = cnf.x_left;
    for i in 1..cnf.node {
        xs[i] = xs[i - 1] + cnf.dx;
    }

    let mut us = vec![0.0; cnf.node];
    match cnf.init {
        Init::Sin => {
            for i in 0..cnf.node {
                us[i] = 0.5 * (1.1 + f64::sin(cnf.c * (xs[i])));
            }
        }
        _ => {
            us = vec![0.1; cnf.node];
            for i in (cnf.i_max / 2 - 10)..(cnf.i_max / 2 + 10) {
                us[i] = 1.0;
            }
        }
    }

    (xs, us)
}

pub fn exact(cnf: &Config, t: f64, xs: &Vec<f64>) -> Vec<f64> {
    let mut ues = vec![0.0; cnf.node];
    match cnf.init {
        Init::Sin => {
            for i in 0..cnf.node {
                ues[i] = 0.5 * (1.1 + f64::sin(cnf.c * (xs[i] - cnf.a * t)));
            }
        }
        _ => ues = exact_square(&cnf, t),
    };

    ues
}

fn exact_square(cnf: &Config, t: f64) -> Vec<f64> {
    let mut ues = vec![0.1; cnf.node];
    let xc = cnf.a * t;

    let mut xl = xc - 10.0 * cnf.dx;
    if xl > 1.0 - 2.0 * cnf.dx {
        xl = -2.0 + xl + 2.0 * cnf.dx;
    }

    let mut xr = xc + 10.0 * cnf.dx;
    if xr > 1.0 - 2.0 * cnf.dx {
        xr = -2.0 + xr + 3.0 * cnf.dx;
    }

    if xl <= xr {
        for i in 0..cnf.node {
            if ((i as f64) - (cnf.i_max as f64) / 2.0) * cnf.dx >= xl
                && ((i as f64) - (cnf.i_max as f64) / 2.0) * cnf.dx <= xr
            {
                ues[i] = 1.0;
            }
        }
    }

    if xl >= xr {
        for i in 0..cnf.i_max {
            if ((i as f64) - (cnf.i_max as f64) / 2.0) * cnf.dx >= xr
                && ((i as f64) - (cnf.i_max as f64) / 2.0) * cnf.dx <= xl
            {
                ues[i] = 0.1;
            } else {
                ues[i] = 1.0;
            }
        }
    }

    ues
}

pub fn reconstruction(cnf: &Config, us: &Vec<f64>) -> (Vec<f64>, Vec<f64>) {
    let mut uls = vec![0.0; cnf.node];
    let mut urs = vec![0.0; cnf.node];

    for i in 1..(cnf.node - 2) {
        match cnf.reconstruction {
            Reconstruction::PC => {
                uls[i + 1] = us[i];
                urs[i + 1] = us[i + 1];
            }
            Reconstruction::LaxWendorff => {
                let delta_l = 0.5 * (us[i + 1] - us[i]);
                let delta_r = 0.5 * (us[i + 1] - us[i]);
                // セル境界 (i+1/2) 左側の値
                uls[i + 1] = us[i] + (1.0 - cnf.alpha_12 * cnf.dt / cnf.dx) * delta_l;
                // セル境界 (i+1/2) 右側の値
                urs[i + 1] = us[i + 1] - (1.0 + cnf.alpha_12 * cnf.dt / cnf.dx) * delta_r;
            }
            Reconstruction::BeamWarming => {
                let delta_l = 0.5 * (us[i] - us[i - 1]);
                let delta_r = 0.5 * (us[i + 2] - us[i + 1]);
                // セル境界 (i+1/2) 左側の値
                uls[i + 1] = us[i] + (1.0 - cnf.alpha_12 * cnf.dt / cnf.dx) * delta_l;
                // セル境界 (i+1/2) 右側の値
                urs[i + 1] = us[i + 1] - (1.0 + cnf.alpha_12 * cnf.dt / cnf.dx) * delta_r;
            }
            Reconstruction::Fromm => {
                let delta_l = 0.25 * (us[i + 1] - us[i - 1]);
                let delta_r = 0.25 * (us[i + 2] - us[i]);
                // セル境界 (i+1/2) 左側の値
                uls[i + 1] = us[i] + (1.0 - cnf.alpha_12 * cnf.dt / cnf.dx) * delta_l;
                // セル境界 (i+1/2) 右側の値
                urs[i + 1] = us[i + 1] - (1.0 + cnf.alpha_12 * cnf.dt / cnf.dx) * delta_r;
            }
            Reconstruction::TVD => {
                // 0 割り算を避けるための正則化
                let reg_term = sign(us[i + 1] - us[1]) * 1.0e-5;
                match cnf.limiter {
                    Limiter::Minmod => {
                        let r_l = (us[i] - us[i - 1]) / (us[i + 1] - us[i] + reg_term);
                        let r_r = (us[i + 2] - us[i + 1]) / (us[i + 1] - us[i] + reg_term);
                        let phi_l = minmod(r_l);
                        let phi_r = minmod(r_r);
                        let delta_l = 0.5 * (us[i + 1] - us[i]);
                        let delta_r = 0.5 * (us[i + 1] - us[i]);
                        // セル境界 (i+1/2) 左側の値
                        uls[i + 1] =
                            us[i] + (1.0 - cnf.alpha_12 * cnf.dt / cnf.dx) * phi_l * delta_l;
                        // セル境界 (i+1/2) 右側の値
                        urs[i + 1] =
                            us[i + 1] - (1.0 + cnf.alpha_12 * cnf.dt / cnf.dx) * phi_r * delta_r;
                    }
                    Limiter::Superbee => {
                        let r_l = (us[i] - us[i - 1]) / (us[i + 1] - us[i] + reg_term);
                        let r_r = (us[i + 1] - us[i]) / (us[i + 1] - us[i] + reg_term);
                        let phi_l = superbee(r_l);
                        let phi_r = superbee(r_r);
                        let delta_l = 0.5 * (us[i + 1] - us[i]);
                        let delta_r = 0.5 * (us[i + 1] - us[i]);
                        // セル境界 (i+1/2) 左側の値
                        uls[i + 1] =
                            us[i] + (1.0 - cnf.alpha_12 * cnf.dt / cnf.dx) * phi_l * delta_l;
                        // セル境界 (i+1/2) 右側の値
                        urs[i + 1] =
                            us[i + 1] - (1.0 + cnf.alpha_12 * cnf.dt / cnf.dx) * phi_r * delta_r;
                    }
                    Limiter::VanLeer => {
                        let r_l = (us[i] - us[i - 1]) / (us[i + 1] - us[i] + reg_term);
                        let r_r = (us[i + 2] - us[i + 1]) / (us[i + 1] - us[i] + reg_term);
                        let phi_l = van_leer(r_l);
                        let phi_r = van_leer(r_r);
                        let delta_l = 0.5 * (us[i + 1] - us[i]);
                        let delta_r = 0.5 * (us[i + 1] - us[i]);
                        uls[i + 1] =
                            us[i] + (1.0 - cnf.alpha_12 * cnf.dt / cnf.dx) * phi_l * delta_l;
                        urs[i + 1] =
                            us[i + 1] + (1.0 - cnf.alpha_12 * cnf.dt / cnf.dx) * phi_r * delta_r;
                    }
                    Limiter::VanAlbada => {
                        let r_l = (us[i] - us[i - 1]) / (us[i + 1] - us[i] + reg_term);
                        let r_r = (us[i + 2] - us[i + 1]) / (us[i + 1] - us[i] + reg_term);
                        let phi_l = van_albada(r_l);
                        let phi_r = van_albada(r_r);
                        let delta_l = 0.5 * (us[i + 1] - us[i]);
                        let delta_r = 0.5 * (us[i + 1] - us[i]);
                        // セル境界 (i+1/2) 左側の値
                        uls[i + 1] =
                            us[i] + (1.0 - cnf.alpha_12 * cnf.dt / cnf.dx) * phi_l * delta_l;
                        // セル境界 (i+1/2) 右側の値
                        urs[i + 1] =
                            us[i + 1] - (1.0 + cnf.alpha_12 * cnf.dt / cnf.dx) * phi_r * delta_r;
                    }
                }
            }
        }
    }

    (uls, urs)
}

fn minmod(x: f64) -> f64 {
    f64::max(0.0, f64::min(1.0, x))
}

fn superbee(x: f64) -> f64 {
    f64::max(f64::max(0.0, f64::min(1.0, 2.0 * x)), f64::min(2.0, x))
}

fn van_leer(x: f64) -> f64 {
    (x + f64::abs(x)) / (1.0 + f64::abs(x))
}

fn van_albada(x: f64) -> f64 {
    (x + x * x) / (1.0 + x * x)
}

fn sign(x: f64) -> f64 {
    if x >= 0.0 {
        1.0
    } else {
        -1.0
    }
}

pub fn riemann(cnf: &Config, uls: &Vec<f64>, urs: &Vec<f64>) -> Vec<f64> {
    let mut fs = vec![0.0; cnf.node];

    match cnf.riemann {
        Riemann::Roe => {
            for i in 2..(cnf.node - 1) {
                fs[i] = 1.0 / 2.0 * (cnf.a * uls[i] + cnf.a * urs[i])
                    - 1.0 / 2.0 * f64::abs(cnf.a) * (urs[i] - uls[i]);
            }
        }
    }

    fs
}

pub fn update(cnf: &Config, us: &Vec<f64>, fs: &Vec<f64>) -> Vec<f64> {
    let mut us_new = us.to_vec();

    for i in 2..(cnf.node - 2) {
        us_new[i] = us[i] - cnf.dt / cnf.dx * (fs[i + 1] - fs[i]);
    }

    // bc
    us_new[0] = us_new[cnf.i_max - 3];
    us_new[1] = us_new[cnf.i_max - 2];
    us_new[cnf.i_max - 1] = us_new[2];
    us_new[cnf.i_max] = us_new[3];

    us_new
}

pub fn write_file(
    dir_name: &str,
    n: &i64,
    node: &usize,
    xs: &Vec<f64>,
    us: &Vec<f64>,
    ues: &Vec<f64>,
    uls: &Vec<f64>,
    urs: &Vec<f64>,
    fs: &Vec<f64>,
) -> Result<(), Box<dyn error::Error>> {
    let file_name: String = format!("{}/{:08}.csv", &dir_name, &n);
    let file = File::create(file_name).unwrap();
    let mut w = BufWriter::new(file);
    write!(w, "xs,us,ues,uls,urs,fs\n").unwrap();
    for i in 0..*node {
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
