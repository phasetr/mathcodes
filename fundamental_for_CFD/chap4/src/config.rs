extern crate yaml_rust;
use crate::init::Init;
use crate::limiter as lim;
use crate::reconstruction as rec;
use crate::riemann_solver as riem;
use std::fs::File;
use std::io::prelude::*;
use yaml_rust::YamlLoader;
const PI: f64 = std::f64::consts::PI;

pub struct Config {
    pub init: Init,
    pub init_string: String,
    pub reconstruction: rec::Reconstruction,
    pub reconstruction_string: String,
    pub limiter: lim::Limiter,
    pub limiter_string: String,
    pub riemann: riem::Riemann,
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
        let init_string: String = cnf_yaml["init"].as_str().unwrap().to_string();
        let init: Init = Init::new(&init_string);

        // reconstruction の置換処理
        let reconstruction_string: String =
            cnf_yaml["reconstruction"].as_str().unwrap().to_string();
        let reconstruction: rec::Reconstruction = rec::Reconstruction::new(&reconstruction_string);

        // limiter の置換処理
        let limiter_string: String = cnf_yaml["limiter"].as_str().unwrap().to_string();
        let limiter: lim::Limiter = lim::Limiter::new(&limiter_string);

        // riemann の置換処理
        let riemann_string: String = cnf_yaml["riemann"].as_str().unwrap().to_string();
        let riemann: riem::Riemann = riem::Riemann::new(&riemann_string);

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
