// cargo-deps: chrono
extern crate chrono;
use chrono::Local;
use std::error;
use std::fs;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufWriter;

const I_MAX: usize = 100;
const NODE: usize = I_MAX + 1;
const PI: f64 = std::f64::consts::PI;

fn write_file(
    dir_name: &str,
    t: &f64,
    xs: &[f64],
    us: &[f64],
    ues: &[f64],
    uls: &[f64],
    urs: &[f64],
    fs: &[f64],
) -> Result<(), Box<dyn error::Error>> {
    let file_name: String = format!("{}/{}", &dir_name, &t.to_string());
    //        println!("{}", &file_name);
    let file = File::create(file_name).unwrap();
    let mut w = BufWriter::new(file);
    write!(w, "xs,us,ues,uls,urs,fs\n").unwrap();
    for i in 0..NODE {
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

fn f_flux(x: f64) -> f64 {
    0.5 * x * x
}

fn main() {
    let x_left: f64 = -1.0;
    let x_right: f64 = 1.0;
    let tstop: f64 = 2.0;
    let a: f64 = 1.0;
    let c: f64 = 2.0 * PI * ((I_MAX as f64) + 3.0) / (I_MAX as f64);
    let dx: f64 = (x_right - x_left) / (I_MAX as f64);
    let dt: f64 = 0.2 * dx;

    let dir_name = Local::now()
        .format("workspace/%Y%m%d-%H%M%S_4-2_convection_smooth.rs")
        .to_string();

    fs::create_dir_all(&dir_name).unwrap_or_else(|why| {
        println!("! {:?}", why.kind());
    });

    let mut n: i64 = 0;
    let mut t: f64 = 0.0;

    // initc
    let mut xs = [0.0; NODE];
    xs[0] = x_left;
    for i in 1..NODE {
        xs[i] = xs[i - 1] + dx;
    }

    let mut us = [0.0; NODE];
    for i in 0..NODE {
        us[i] = 0.5 * (1.1 + f64::sin(c * (xs[i] - a * t)));
    }

    // exact
    let mut ues = [0.0; NODE];
    for i in 0..NODE {
        ues[i] = 0.5 * (1.1 + f64::sin(c * (xs[i] - a * t)));
    }

    let mut uls = [0.0; NODE];
    let mut urs = [0.0; NODE];
    let mut fs = [0.0; NODE];
    write_file(&dir_name, &t, &xs, &us, &ues, &uls, &urs, &fs);

    while t <= tstop {
        n = n + 1;
        t = t + dt;

        // reconstruction_pc
        for i in 1..(NODE - 2) {
            uls[i + 1] = us[i];
            urs[i + 1] = us[i + 1];
        }

        // riemann_roe
        let mut alpha_12: f64 = 0.0;
        for i in 2..(NODE - 1) {
            alpha_12 = 0.5 * (urs[i] - uls[i]);
            fs[i] = 1.0 / 2.0 * (f_flux(uls[i]) + f_flux(urs[i]))
                - 1.0 / 2.0 * f64::abs(alpha_12) * (urs[i] - uls[i]);
        }

        // update
        for i in 2..(NODE - 2) {
            us[i] = us[i] - dt / dx * (fs[i + 1] - fs[i]);
        }

        // gc
        us[0] = us[I_MAX - 3];
        us[1] = us[I_MAX - 2];
        us[I_MAX - 1] = us[2];
        us[I_MAX] = us[3];

        // exact
        for i in 0..NODE {
            let mut f = ues[i] - 0.5 * (1.1 + f64::sin(c * (xs[i] - ues[i] * t)));
            let mut df = 1.0 + 0.5 * c * f64::cos(c * (xs[i] - ues[i] * t)) * t;
            while f64::abs(f) >= 1.0e-4 {
                ues[i] = ues[i] - f / df; // ニュートン法の計算式
                f = ues[i] - 0.5 * (1.1 + f64::sin(c * (xs[i] - ues[i] * t)));
                df = 1.0 + 0.5 * c * f64::cos(c * (xs[i] - ues[i] * t)) * t;
            }
        }

        write_file(&dir_name, &t, &xs, &us, &ues, &uls, &urs, &fs);
    }
    println!("1d_mp4.py を実行して動画ファイルを生成してください.");
}
