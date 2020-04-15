// cargo-deps: chrono, ndarray, rand
// 参考 https://qiita.com/akoamay/items/50ecc312cd84596203c1
extern crate chrono;
extern crate ndarray;
extern crate rand;
use chrono::Local;
use ndarray::prelude::*;
use ndarray::Array1;
use rand::Rng;
use std::error;
use std::fs;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufWriter;
use std::process;
use std::process::Command;
use std::time::Instant;

macro_rules! measure {
    ( $x:expr) => {{
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

struct Config {
    pub x_min: f64,
    pub x_max: f64,
    pub y_min: f64,
    pub y_max: f64,
    pub t_max: f64, // 計算終了時刻
    pub dx: f64,
    pub dy: f64,
    pub dt: f64,
    pub nt: u64,          // 計算ステップ数
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
        let x_min = -10.0;
        let x_max = 10.0;
        let y_min = -10.0;
        let y_max = 10.0;
        let dx = 0.1;
        let dy = 0.1;
        let dt = 0.005;
        let nt = 1000;
        let t_max = (nt as f64) / dt;
        let output_step = 1;
        let trial_num = 5000;
        let total_step = trial_num * nt;

        let prog_name = "ode-dynamical-system-006";
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
    pub bound_flg: bool,
    pub output_num: u64,
    pub tmp_total_num: usize,
    pub x: Array1<f64>,
    pub y: Array1<f64>,
}

impl CalcData {
    pub fn new(cnf: &Config) -> CalcData {
        let n: usize = 0;
        let t: f64 = 0.0;
        let bound_flg: bool = true;
        let output_num: u64 = 0;
        let tmp_total_num: usize = 0;

        let x: Array1<f64> = Array::zeros(cnf.nt as usize);
        let y: Array1<f64> = Array::zeros(cnf.nt as usize);
        CalcData {
            n: n,
            t: t,
            bound_flg: bound_flg,
            output_num: output_num,
            tmp_total_num: tmp_total_num,
            x: x,
            y: y,
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
        write!(w, "x,y\n").unwrap();
        for i in 0..(cdata.x.len()) {
            let s = format!("{},{}\n", &cdata.x[i], &cdata.y[i]);
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
            .arg(format!(r#"set xrange [{}:{}]"#, &cnf.x_min, &cnf.x_max))
            .arg("-e")
            .arg(format!(r#"set yrange [{}:{}]"#, &cnf.y_min, &cnf.y_max))
            .arg("-e")
            .arg(format!(r#"set output "{}""#, &png_name))
            .arg("-e")
            .arg(format!(
                r#"plot "{}" u 1:2 pt 7 ps 0.1 lc rgb '#99ccff';"#,
                &csv_name
            ))
            .output()
            .expect("failed to start `gnuplot`");
        Ok(())
    }

    pub fn write_mp4(cnf: &Config) -> Result<(), Box<dyn error::Error>> {
        let options = ["-r", "10", "-i", &cnf.png_file_fmt, &cnf.movie_name];

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

    // initc
    let mut cdata = CalcData::new(&cnf);

    // 初期状態の記録
    CalcData::write_csv(&cnf, &cdata)
        .map_err(|err| println!("{:?}", err))
        .ok();

    measure!({
        for trial in 0..cnf.trial_num {
            println!("\nNow trial {}/{}", trial, cnf.trial_num);

            // 再処理のための初期化
            cdata.n = 0;
            cdata.t = 0.0;
            cdata.bound_flg = true;

            let mut rng = rand::thread_rng();
            let x0: f64 = rng.gen_range(cnf.x_min, cnf.x_max);
            let y0: f64 = rng.gen_range(cnf.y_min, cnf.y_max);
            println!("(x0, y0) = ({}, {})", x0, y0);
            cdata.x = Array::zeros(cnf.nt as usize);
            cdata.y = Array::zeros(cnf.nt as usize);
            cdata.x[cdata.n] = x0;
            cdata.y[cdata.n] = y0;

            while ((cdata.n as u64) < cnf.nt - 1) && cdata.bound_flg {
                // カウントアップ
                cdata.n += 1;
                cdata.t = cdata.t + cnf.dt;

                cdata.x[cdata.n] = cdata.x[cdata.n - 1] + cnf.dt * (-1.0 * cdata.x[cdata.n - 1]);
                cdata.y[cdata.n] = cdata.y[cdata.n - 1] + cnf.dt * (-2.0 * cdata.y[cdata.n - 1]);

                if bound_check(&cnf, &cdata) {
                    cdata.bound_flg = false;
                }
            }

            // 1 曲線ごとに保存
            CalcData::write_csv(&cnf, &cdata)
                .map_err(|err| println!("{:?}", err))
                .ok();
            cdata.output_num += 1;
        }
    });
    println!("python3 ode_dynamical_system002_visualize.py で可視化してください. 必要なら適当なところで処理を止めるといいでしょう.");
}

fn bound_check(cnf: &Config, cdata: &CalcData) -> bool {
    cdata.x[cdata.n] < cnf.x_min
        || cnf.x_max < cdata.x[cdata.n]
        || cdata.y[cdata.n] < cnf.y_min
        || cnf.y_max < cdata.y[cdata.n]
}
