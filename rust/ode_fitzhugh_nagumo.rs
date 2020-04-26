// cargo-deps: chrono, ndarray
// 参考 https://qiita.com/akoamay/items/50ecc312cd84596203c1
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
use std::time::Instant;

macro_rules! measure {
    ($x:expr) => {{
        let start = Instant::now();
        let result = $x;
        let end = start.elapsed();
        println!(
            "計測開始から{}.{:03}秒経過しました。",
            end.as_secs(),
            end.subsec_nanos() / 1_000_000
        );
        result
    }};
}

pub enum CoordType {
    X,
    Y,
}

struct Config {
    pub x_min: f64,
    pub x_max: f64,
    pub y_min: f64,
    pub y_max: f64,
    pub t_max: f64, // 計算終了時刻
    pub dx: f64,
    pub dy: f64,
    pub dt: f64,
    pub nt: u64, // 計算ステップ数
    pub a: f64,
    pub b: f64,
    pub c: f64,
    pub trial_num: u64,   // ランダムな初期値を選ぶ回数
    pub total_step: u64,  // 全計算回数
    pub output_step: u64, // 出力ステップ数
    pub csv_dir_name: String,
    pub png_dir_name: String,
    pub png_file_fmt: String,
    pub movie_name: String,
}

impl Config {
    pub fn new() -> Result<Config, &'static str> {
        let x_min = -2.2;
        let x_max = 2.2;
        let y_min = -0.5;
        let y_max = 1.5;
        let dx = 0.1;
        let dy = 0.1;
        let dt = 0.01;
        let nt = 1000;
        let t_max = (nt as f64) / dt;
        let a = 0.7;
        let b = 0.8;
        let c = 10.0;
        let output_step = 1;
        let trial_num = 20;
        let total_step = trial_num * nt;

        let prog_name = "ode-fitzhugh-nagumo";
        let dir_root_name = Local::now()
            .format(&format!("%Y%m%d-%H%M%S-{}", &prog_name))
            .to_string();
        let csv_dir_name = format!("workspace/{}_csv", &dir_root_name);
        let png_dir_name = format!("workspace/{}_png", &dir_root_name);

        let png_file_fmt = format!("{}/img.%08d.png", &png_dir_name);
        let movie_name = format!("workspace/{}.tmp.mp4", &dir_root_name);

        Ok(Config {
            x_min: x_min,
            x_max: x_max,
            y_min: y_min,
            y_max: y_max,
            t_max: t_max,
            dx: dx,
            dy: dy,
            dt: dt,
            nt: nt,
            a: a,
            b: b,
            c: c,
            output_step: output_step,
            trial_num: trial_num,
            total_step: total_step,
            csv_dir_name: csv_dir_name,
            png_dir_name: png_dir_name,
            png_file_fmt: png_file_fmt,
            movie_name: movie_name,
        })
    }
}

struct CalcData {
    pub n: usize,
    pub t: f64,
    pub i_const: f64,
    pub bound_flg: bool,
    pub output_num: u64,
    pub tmp_total_num: usize,
    pub coordx: Array1<f64>,
    pub coordy: Array1<f64>,
    pub x: Array1<f64>,
    pub y: Array1<f64>,
    pub u: Array2<f64>,
    pub v: Array2<f64>,
}

impl CalcData {
    pub fn new(cnf: &Config) -> CalcData {
        let n: usize = 0;
        let t: f64 = 0.0;
        let i_const: f64 = 0.34;

        let bound_flg: bool = true;
        let output_num: u64 = 0;
        let tmp_total_num: usize = 0;

        let x: Array1<f64> = Array::zeros(cnf.nt as usize);
        let y: Array1<f64> = Array::zeros(cnf.nt as usize);

        let coordx: Array1<f64> = Array::range(cnf.x_min, cnf.x_max, cnf.dx);
        let coordy: Array1<f64> = Array::range(cnf.y_min, cnf.y_max, cnf.dy);
        let u: Array2<f64> = Array::zeros((coordx.len(), coordy.len()));
        let v: Array2<f64> = Array::zeros((coordx.len(), coordy.len()));

        CalcData {
            n: n,
            t: t,
            i_const: i_const,
            bound_flg: bound_flg,
            output_num: output_num,
            tmp_total_num: tmp_total_num,
            coordx: coordx,
            coordy: coordy,
            x: x,
            y: y,
            u: u,
            v: v,
        }
    }

    pub fn i(cdata: &CalcData, t: &f64) -> f64 {
        cdata.i_const
    }

    pub fn write_csv(cnf: &Config, cdata: &CalcData) -> Result<(), Box<dyn error::Error>> {
        let file_name: String = format!("{}/{:08}.csv", &cnf.csv_dir_name, &cdata.output_num);
        let file = File::create(file_name).unwrap();
        let mut w = BufWriter::new(file);
        write!(w, "x,y\n").unwrap();
        for i in 0..(cdata.x.len()) {
            let s = format!("{},{}\n", &cdata.x[i], &cdata.y[i]);
            write!(w, "{}", s).unwrap();
        }
        w.flush().unwrap();

        let vf_file_name: String = format!("{}/{:08}.vf.csv", &cnf.csv_dir_name, &cdata.output_num);
        let vf_file = File::create(vf_file_name).unwrap();
        let mut w = BufWriter::new(vf_file);
        write!(w, "x,y,u,v\n").unwrap();
        for i in 0..(cdata.coordx.len()) {
            for j in 0..(cdata.coordy.len()) {
                let s = format!(
                    "{},{},{},{}\n",
                    &cdata.coordx[i],
                    &cdata.coordy[j],
                    &cdata.u[(i, j)],
                    &cdata.v[(i, j)],
                );
                write!(w, "{}", s).unwrap();
            }
        }
        w.flush().unwrap();

        Ok(())
    }

    pub fn write_mp4() -> Result<(), Box<dyn error::Error>> {
        let run_ffmpeg = Command::new("python")
            .arg("ode_fitzhugh_nagumo_visualize.py")
            .output()
            .expect("failed to start `python`");
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

    // initc
    let mut cdata = CalcData::new(&cnf);

    // 初期状態の記録
    CalcData::write_csv(&cnf, &cdata)
        .map_err(|err| println!("{:?}", err))
        .ok();

    measure!({
        for trial in 0..cnf.trial_num {
            println!("Now trial {}/{}", trial, cnf.trial_num);

            cdata.i_const = 0.20 + (trial as f64) * 0.01;

            // ベクトル場の計算
            for i in 0..cdata.coordx.len() {
                for j in 0..cdata.coordy.len() {
                    cdata.u[(i, j)] = cdata.coordx[i] - cnf.b * cdata.coordy[j] + cnf.a;
                    cdata.v[(i, j)] = cnf.c
                        * (-cdata.coordy[j] + cdata.coordx[i] - cdata.coordx[i].powf(3.0) / 3.0
                            + CalcData::i(&cdata, &0.0));
                }
            }

            // 再処理のための初期化
            cdata.n = 0;
            cdata.t = 0.0;
            cdata.bound_flg = true;

            cdata.x = Array::zeros(cnf.nt as usize);
            cdata.y = Array::zeros(cnf.nt as usize);
            cdata.x[0] = 2.0;
            cdata.y[0] = 1.0;

            while (cdata.n as u64) < cnf.nt - 1 {
                // カウントアップ
                cdata.n += 1;
                cdata.t = cdata.t + cnf.dt;

                cdata.y[cdata.n] = rk(CoordType::Y, &cnf, &cdata, fy);
                cdata.x[cdata.n] = rk(CoordType::X, &cnf, &cdata, fx);
            }

            // 1 曲線ごとに保存
            CalcData::write_csv(&cnf, &cdata)
                .map_err(|err| println!("{:?}", err))
                .ok();
            cdata.output_num += 1;
        }
    });

    CalcData::write_mp4()
        .map_err(|err| println!("{:?}", err))
        .ok();
    println!("python3 ode_fitzhugh_nagumo_visualize.py を実行して png・mp4 を生成してください.");
}

fn rk(
    ctype: CoordType,
    cnf: &Config,
    cdata: &CalcData,
    f: fn(&Config, &CalcData, &f64, &f64, &f64) -> f64,
) -> f64 {
    let x = cdata.x[cdata.n - 1];
    let y = cdata.y[cdata.n - 1];
    let t = cdata.t - cnf.dt;
    let half_t = 0.5 * cnf.dt;
    let k1 = f(&cnf, &cdata, &t, &x, &y);
    let k2 = f(
        &cnf,
        &cdata,
        &(&t + half_t),
        &(&x + half_t * k1),
        &(&y + half_t * k1),
    );
    let k3 = f(
        &cnf,
        &cdata,
        &(&t + half_t),
        &(&x + half_t * k2),
        &(&y + half_t * k2),
    );
    let k4 = f(&cnf, &cdata, &t, &(&x + cnf.dt * k3), &(&y + cnf.dt * k3));

    let nth = match ctype {
        CoordType::X => x,
        _ => y,
    };
    nth + (cnf.dt / 6.0) * (k1 + 2.0 * k2 + 2.0 * k3 + k4)
}

fn fx(cnf: &Config, cdata: &CalcData, t: &f64, x: &f64, y: &f64) -> f64 {
    cnf.c * (x - x.powf(3.0) / 3.0 - y + CalcData::i(&cdata, t))
}
fn fy(cnf: &Config, cdata: &CalcData, t: &f64, x: &f64, y: &f64) -> f64 {
    x - cnf.b * y + cnf.a
}
