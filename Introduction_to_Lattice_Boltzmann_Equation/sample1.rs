// cargo-deps: chrono, ndarray
extern crate chrono;
extern crate ndarray;
use chrono::Local;
//use ndarray::prelude::*;
use ndarray::Array;
use ndarray::Array2;
use std::error;
use std::fs;
//use std::fs::File;
//use std::io::prelude::*;
//use std::io::BufWriter;
use std::process;
use std::process::Command;

struct Config {
    pub lx: f64,
    pub ly: f64,
    pub dt: f64,   // 時間差分間隔 [s]
    pub nx: usize, // 計算点数
    pub ny: usize, // 計算点数
    pub nt: i64,   // 計算ステップ数
    pub viscocity: f64,
    pub omega: f64,
    pub u0: f64,
    pub four_ninth: f64,
    pub one_ninth: f64,
    pub one_thirty_sixth: f64,
    pub output_step: i64, // 出力ステップ数
    pub csv_dir_name: String,
    pub png_dir_name: String,
    pub specify_png: String,
    pub movie_name: String,
    pub graph_ulim_min: f64,
    pub graph_ulim_max: f64,
}

impl Config {
    pub fn new() -> Result<Config, &'static str> {
        let lx = 200.0;
        let ly = 80.0;
        let nx = 200;
        let ny = 80;
        let nt = 10; //5000;
        let dt = 0.01;
        let output_step = 10;

        let viscocity = 0.02;
        let omega = 1.0 / (3.0 * viscocity + 0.5);
        let u0 = 0.1;
        let four_ninth = 4.0 / 9.0;
        let one_ninth = 1.0 / 9.0;
        let one_thirty_sixth = 1.0 / 36.0;

        let graph_ulim_min = -2.0;
        let graph_ulim_max = 2.0;

        let prog_name = "sample1";
        let title = Local::now()
            .format(&format!("%Y%m%d-%H%M%S-{}", &prog_name))
            .to_string();
        let csv_dir_name = format!("workspace/{}_csv", &title);
        let png_dir_name = format!("workspace/{}_png", &title);

        let specify_png = format!("{}/img.%08d.png", &png_dir_name);
        let movie_name = format!("workspace/{}.tmp.mp4", &title);

        Ok(Config {
            lx: lx,
            ly: ly,
            dt: dt,
            nx: nx,
            ny: ny,
            nt: nt,
            output_step: output_step,
            viscocity: viscocity,
            omega: omega,
            u0: u0,
            four_ninth: four_ninth,
            one_ninth: one_ninth,
            one_thirty_sixth: one_thirty_sixth,
            csv_dir_name: csv_dir_name,
            png_dir_name: png_dir_name,
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
    pub n0: Array2<f64>,
    pub nn: Array2<f64>,
    pub ns: Array2<f64>,
    pub ne: Array2<f64>,
    pub nw: Array2<f64>,
    pub nne: Array2<f64>,
    pub nse: Array2<f64>,
    pub nnw: Array2<f64>,
    pub nsw: Array2<f64>,
    pub rho: Array2<f64>,
    pub ux: Array2<f64>,
    pub uy: Array2<f64>,
    pub barrier: Array2<bool>,
    pub barriern: Array2<bool>,
    pub barriers: Array2<bool>,
    pub barriere: Array2<bool>,
    pub barrierw: Array2<bool>,
    pub barrierne: Array2<bool>,
    pub barriernw: Array2<bool>,
    pub barrierse: Array2<bool>,
    pub barriersw: Array2<bool>,
}

impl CalcData {
    pub fn new(cnf: &Config) -> CalcData {
        let n: i64 = 0;
        let t: f64 = 0.0;
        let output_num: i64 = 0;

        let n0: Array2<f64> =
            cnf.four_ninth * Array::from_elem((cnf.nx, cnf.ny), 1.0 - 1.5 * cnf.u0.powf(2.0));
        let nn: Array2<f64> =
            cnf.one_ninth * Array::from_elem((cnf.nx, cnf.ny), 1.0 - 1.5 * cnf.u0.powf(2.0));
        let ns: Array2<f64> =
            cnf.one_ninth * Array::from_elem((cnf.nx, cnf.ny), 1.0 - 1.5 * cnf.u0.powf(2.0));
        let ne: Array2<f64> = cnf.one_ninth
            * Array::from_elem(
                (cnf.nx, cnf.ny),
                1.0 + 3.0 * cnf.u0 + 4.5 * cnf.u0.powf(2.0) - 1.5 * cnf.u0.powf(2.0),
            );
        let nw: Array2<f64> = cnf.one_ninth
            * Array::from_elem(
                (cnf.nx, cnf.ny),
                1.0 - 3.0 * cnf.u0 + 4.5 * cnf.u0.powf(2.0) - 1.5 * cnf.u0.powf(2.0),
            );
        let nne: Array2<f64> = cnf.one_thirty_sixth
            * Array::from_elem(
                (cnf.nx, cnf.ny),
                1.0 + 3.0 * cnf.u0 + 4.5 * cnf.u0.powf(2.0) - 1.5 * cnf.u0.powf(2.0),
            );
        let nse: Array2<f64> = cnf.one_thirty_sixth
            * Array::from_elem(
                (cnf.nx, cnf.ny),
                1.0 + 3.0 * cnf.u0 + 4.5 * cnf.u0.powf(2.0) - 1.5 * cnf.u0.powf(2.0),
            );
        let nnw: Array2<f64> = cnf.one_thirty_sixth
            * Array::from_elem(
                (cnf.nx, cnf.ny),
                1.0 - 3.0 * cnf.u0 + 4.5 * cnf.u0.powf(2.0) - 1.5 * cnf.u0.powf(2.0),
            );
        let nsw: Array2<f64> = cnf.one_thirty_sixth
            * Array::from_elem(
                (cnf.nx, cnf.ny),
                1.0 - 3.0 * cnf.u0 + 4.5 * cnf.u0.powf(2.0) - 1.5 * cnf.u0.powf(2.0),
            );
        let rho = &n0 + &nn + &ns + &ne + &nw + &nne + &nse + &nnw + &nsw;
        let ux = (&ne + &nne + &nse - &nw - &nnw - &nsw) / &rho;
        let uy = (&nn + &nne + &nnw - &ns - &nse - &nsw) / &rho;

        let mut barrier = Array::from_elem((cnf.nx, cnf.ny), false);
        for j in (cnf.ny / 2 - 8)..(cnf.ny / 2) {
            barrier[(cnf.nx / 2, j)] = true;
        }

        // TODO
        let barriern = Array::from_elem((cnf.nx, cnf.ny), false);
        let barriers = Array::from_elem((cnf.nx, cnf.ny), false);
        let barriere = Array::from_elem((cnf.nx, cnf.ny), false);
        let barrierw = Array::from_elem((cnf.nx, cnf.ny), false);
        let barrierne = Array::from_elem((cnf.nx, cnf.ny), false);
        let barriernw = Array::from_elem((cnf.nx, cnf.ny), false);
        let barrierse = Array::from_elem((cnf.nx, cnf.ny), false);
        let barriersw = Array::from_elem((cnf.nx, cnf.ny), false);

        CalcData {
            n: n,
            t: t,
            output_num: output_num,
            n0: n0,
            nn: nn,
            ns: ns,
            ne: ne,
            nw: nw,
            nne: nne,
            nse: nse,
            nnw: nnw,
            nsw: nsw,
            rho: rho,
            ux: ux,
            uy: uy,
            barrier: barrier,
            barriern: barriern,
            barriers: barriers,
            barriere: barriere,
            barrierw: barrierw,
            barrierne: barrierne,
            barriernw: barriernw,
            barrierse: barrierse,
            barriersw: barriersw,
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
        //let file_name: String = format!("{}/{:08}.csv", &cnf.csv_dir_name, &cdata.output_num);
        //let file = File::create(file_name).unwrap();
        //let mut w = BufWriter::new(file);
        //write!(w, "x,u_pml,u_ord,w_pml,ut_ord\n").unwrap();
        //for i in 0..cnf.nx {
        //    let s = format!(
        //        "{},{},{},{},{}\n",
        //        &cdata.x[i], &cdata.u_pml[i], &cdata.u_ord[i], &cdata.w_pml[i], &cdata.ut_ord[i]
        //    );
        //    // unwrapを呼んで書き込みエラーを検知
        //    write!(w, "{}", s).unwrap();
        //}
        //// flushを呼ぶことで書き込みエラーを全て拾える
        //w.flush().unwrap();
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
    //CalcData::write_all(&cnf, &cdata)
    //    .map_err(|err| println!("{:?}", err))
    //    .ok();

    while cdata.n <= cnf.nt {
        if cdata.n % cnf.output_step == 0 {
            println!("Now {}/{} times!", &cdata.n, &cnf.nt);
        }

        cdata.n = cdata.n + 1;
        cdata.t = cdata.t + cnf.dt;

        if cdata.n % cnf.output_step == 0 {
            //CalcData::write_all(&cnf, &cdata)
            //    .map_err(|err| println!("{:?}", err))
            //    .ok();
            cdata.output_num += 1;
        }
    }

    //CalcData::write_mp4(&cnf)
    //    .map_err(|err| println!("{:?}", err))
    //    .ok();
    //println!("{} として結果を出力しています.", &cnf.movie_name);
}
