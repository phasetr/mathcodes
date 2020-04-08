// cargo-deps: chrono, ndarray
extern crate chrono;
extern crate ndarray;
use chrono::Local;
use ndarray::Array;
use ndarray::Array1;
use std::error;
use std::fs;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufWriter;
use std::process;
use std::process::Command;

const PI: f64 = std::f64::consts::PI;

pub enum RiemannOption {
    FluxVectorSplitting,
    FluxDifferenceSplitting,
}

impl RiemannOption {
    pub fn new(riemann_option_str: &str) -> RiemannOption {
        match riemann_option_str {
            "1" => RiemannOption::FluxVectorSplitting,
            "2" => RiemannOption::FluxDifferenceSplitting,
            _ => RiemannOption::FluxDifferenceSplitting, // TODO バリデート: 対象外ならエラーで即終了にしたい
        }
    }

    pub fn to_string(riemann_option_str: &str) -> String {
        match riemann_option_str {
            "1" => "flux-vector-splitting".to_string(),
            "2" => "flux-difference-splitting".to_string(),
            _ => "ERROR".to_string(), // TODO バリデート: 対象外ならエラーで即終了にしたい
        }
    }
}

struct ExactSolutions {
    pub rhoexact: Array1<f64>,
    pub pexact: Array1<f64>,
    pub uexact: Array1<f64>,
}

struct Config {
    pub i_max: usize, // 格子セル数
    pub node: usize,
    pub dx: f64,         // 格子間隔（等間隔とする）
    pub dt: f64,         // 時間刻み
    pub itmax: i64,      // 反復回数の上限
    pub lambda_max: f64, // 最大特性速度
    pub xl: f64,         // 計算領域左端の座標
    pub xr: f64,         // 計算領域右端の座標
    pub gamma: f64,      // 比熱比（理想気体）
    pub cfl: f64,        // CFL数
    pub tstop: f64,      // 計算終了時刻
    pub nstop: i64,      // output_step
    pub output_step: i64,
    pub a: f64,
    pub alpha_12: f64,
    pub epsilon: f64,
    pub csv_dir_name: String,
    pub png_dir_name: String,
    pub title: String,
    pub specify_png: String,
    pub movie_name: String,
    pub graph_xlim_min: f64,
    pub graph_xlim_max: f64,
    pub graph_ulim_min: f64,
    pub graph_ulim_max: f64,
}

impl Config {
    pub fn new() -> Result<Config, &'static str> {
        let i_max = 100;
        let xl = -1.0;
        let xr = 1.0;
        let gamma = 1.4;
        let cfl = 0.4;
        let dx = (xr - xl) / (i_max as f64);
        let dt = 0.2 * dx;
        let itmax = 20;
        let tstop = 0.48;
        let nstop = (tstop / dt) as i64;
        let output_step = 1;

        let graph_ulim_min = 0.0;
        let graph_ulim_max = 1.2;

        let prog_name = "5-2-1dim-euler-Lax-Wendorff";
        let title = Local::now()
            .format(&format!("%Y%m%d-%H%M%S-{}", &prog_name))
            .to_string();
        let csv_dir_name = format!("workspace/{}_csv", &title);
        let png_dir_name = format!("workspace/{}_png", &title);

        let specify_png = format!("{}/img.%08d.png", &png_dir_name);
        let movie_name = format!("workspace/{}.tmp.mp4", &title);

        Ok(Config {
            i_max: i_max,
            node: i_max + 1,
            xl: xl,
            xr: xr,
            gamma: gamma,
            cfl: cfl,
            tstop: tstop,
            nstop: nstop,
            output_step: output_step,
            a: 1.0,
            alpha_12: 1.0,
            epsilon: 0.15,
            dx: dx,
            dt: dt,
            itmax: itmax,
            lambda_max: 0.0,
            csv_dir_name: csv_dir_name,
            png_dir_name: png_dir_name,
            title: title,
            specify_png: specify_png,
            movie_name: movie_name,
            graph_xlim_min: xl,
            graph_xlim_max: xr,
            graph_ulim_min: graph_ulim_min,
            graph_ulim_max: graph_ulim_max,
        })
    }
}

struct CalcData {
    pub n: i64, // 時間ステップ
    pub t: f64, // 計算時間
    pub output_num: i64,
    pub x: Array1<f64>,        // セル境界の座標
    pub rho: Array1<f64>,      // 密度“rho”のセル平均値
    pub rhou: Array1<f64>,     // 運動量“rho・u”のセル平均値
    pub rhoe: Array1<f64>,     // 総エネルギー“rho・E”のセル平均値
    pub u: Array1<f64>,        // 速度“u”のセル平均値
    pub p: Array1<f64>,        // 圧力“p”のセル平均値
    pub rhoexact: Array1<f64>, // 各格子上における密度の厳密解
    pub uexact: Array1<f64>,   // 各格子上における速度の厳密解
    pub pexact: Array1<f64>,   // 各格子上における圧力の厳密解
    // 空間再構築から求めたセル境界左・右側の保存変数
    pub rho_l: Array1<f64>,  // 左側の密度“rho^L”
    pub rho_r: Array1<f64>,  // 右側の密度“rho^R”
    pub rhou_l: Array1<f64>, // 左側の運動量“(rho・u)^L”
    pub rhou_r: Array1<f64>, // 右側の運動量“(rho・u)^R”
    pub rhoe_l: Array1<f64>, // 左側の総エネルギー“(rho・E)^L”
    pub rhoe_r: Array1<f64>, // 右側の総エネルギー“(rho・E)^R”
    // セル境界における数値流束
    pub f1: Array1<f64>,        // 密度流束
    pub f2: Array1<f64>,        // 運動量流束
    pub f3: Array1<f64>,        // 全エネルギー流束
    pub lh_rho: Array1<f64>,    // 密度の空間離散式
    pub lh_rhou: Array1<f64>,   // 運動量の空間離散式
    pub lh_rhoe: Array1<f64>,   // 総エネルギーの空間離散式
    pub rho_star: Array1<f64>,  // 密度の中間値
    pub rhou_star: Array1<f64>, // 運動量の中間値
    pub rhoe_star: Array1<f64>, // 総エネルギーの中間値
}

impl CalcData {
    pub fn new(cnf: &Config) -> CalcData {
        let n: i64 = 0;
        let t: f64 = 0.0;
        let output_num: i64 = 0;

        // initc
        let mut x = Array::zeros(cnf.node);
        x[0] = cnf.xl;
        for i in 1..cnf.node {
            x[i] = x[i - 1] + cnf.dx;
        }

        let mut rho = Array::zeros(cnf.node - 1);
        let mut rhou = Array::zeros(cnf.node - 1);
        let mut rhoe = Array::zeros(cnf.node - 1);
        let mut u = Array::zeros(cnf.node - 1);
        let mut p = Array::zeros(cnf.node - 1);

        let rho_l = Array::zeros(cnf.node);
        let rho_r = Array::zeros(cnf.node);
        let rhou_l = Array::zeros(cnf.node);
        let rhou_r = Array::zeros(cnf.node);
        let rhoe_l = Array::zeros(cnf.node);
        let rhoe_r = Array::zeros(cnf.node);

        let f1 = Array::zeros(cnf.node);
        let f2 = Array::zeros(cnf.node);
        let f3 = Array::zeros(cnf.node);

        let lh_rho = Array::zeros(cnf.node - 1);
        let lh_rhou = Array::zeros(cnf.node - 1);
        let lh_rhoe = Array::zeros(cnf.node - 1);

        let rho_star = Array::zeros(cnf.node - 1);
        let rhou_star = Array::zeros(cnf.node - 1);
        let rhoe_star = Array::zeros(cnf.node - 1);

        for i in 0..(cnf.node - 1) {
            if x[i] <= 0.0 {
                rho[i] = 1.0;
                u[i] = 0.0; // 不要だがプログラムを比較しやすくするために入れておく
                p[i] = 1.0;
                rhou[i] = rho[i] * u[i];
                rhoe[i] = 0.5 * rhou[i] * u[i] + p[i] / (cnf.gamma - 1.0);
            } else {
                rho[i] = 0.125;
                u[i] = 0.0; // 不要だがプログラムを比較しやすくするために入れておく
                p[i] = 0.1;
                rhou[i] = rho[i] * u[i];
                rhoe[i] = 0.5 * rhou[i] * u[i] + p[i] / (cnf.gamma - 1.0);
            }
        }

        let rhoexact = Array::zeros(cnf.node - 1);
        let uexact = Array::zeros(cnf.node - 1);
        let pexact = Array::zeros(cnf.node - 1);

        CalcData {
            n: n,
            t: t,
            output_num: output_num,
            x: x,
            rho: rho,
            rhoe: rhoe,
            rhou: rhou,
            u: u,
            p: p,
            rhoexact: rhoexact,
            uexact: uexact,
            pexact: pexact,
            rho_l: rho_l,
            rho_r: rho_r,
            rhou_l: rhou_l,
            rhou_r: rhou_r,
            rhoe_l: rhoe_l,
            rhoe_r: rhoe_r,
            f1: f1,
            f2: f2,
            f3: f3,
            lh_rho: lh_rho,
            lh_rhou: lh_rhou,
            lh_rhoe: lh_rhoe,
            rho_star: rho_star,
            rhou_star: rhou_star,
            rhoe_star: rhoe_star,
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
        let file_name: String = format!("{}/{:08}.csv", &cnf.csv_dir_name, &cdata.n);
        let file = File::create(file_name).unwrap();
        let mut w = BufWriter::new(file);
        write!(w, "x,u,rho,p,uexact,rhoexact,pexact,rhou_star,f1\n").unwrap();
        for i in 0..(cnf.node - 1) {
            let s = format!(
                "{},{},{},{},{},{},{},{},{}\n",
                &cdata.x[i],
                &cdata.u[i],
                &cdata.rho[i],
                &cdata.p[i],
                &cdata.uexact[i],
                &cdata.rhoexact[i],
                &cdata.pexact[i],
                &cdata.rhou_star[i],
                &cdata.f1[i]
            );
            // unwrap を呼んで書き込みエラーを検知
            write!(w, "{}", s).unwrap();
        }
        // flush を呼ぶことで書き込みエラーを全て拾える
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
            .arg(format!(
                r#"set yrange [{}:{}]"#,
                &cnf.graph_ulim_min, &cnf.graph_ulim_max
            ))
            .arg("-e")
            .arg(format!(r#"set output "{}""#, &png_name))
            .arg("-e")
            .arg(format!(
                r#"plot "{}" using 1:3 title "rho numerical" with lines lw 2.5, "{}" using 1:6 title "rho exact" with lines lw 2.5;"#,
                &csv_name,
                &csv_name
            ))
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

fn exact_solution(cnf: &Config, cdata: &CalcData) -> ExactSolutions {
    let mut uexact: Array1<f64> = Array::zeros(cnf.node);
    let mut rhoexact: Array1<f64> = Array::zeros(cnf.node);
    let mut pexact: Array1<f64> = Array::zeros(cnf.node);

    let x0 = 0.0; // 初期区切りの位置

    // 初期値から領域 1 の変数値を与える
    let rho1 = 0.125; // 密度
    let p1 = 0.1; // 圧力
    let u1 = 0.0; // 流速
    let c1 = f64::sqrt(cnf.gamma * p1 / rho1); // 音速

    // 初期値から領域 5 の変数値を与える
    let rho5 = 1.0;
    let p5 = 1.0;
    let u5 = 0.0;
    let c5 = f64::sqrt(cnf.gamma * p5 / rho5);

    // Secant 反復法により, 衝撃波前後の圧力ジャンプを求める
    let mut p21 = p1 / p5;
    let mut pm = 21.0 + 0.01;
    let mut pmm = pm + 0.01;
    let mut fm = 0.0;
    let mut df = 0.0;
    let mut fmm = capital_f(&cnf, pm, p1, p5, rho1, rho5, u1, u5);
    let mut it = 0;
    let mut error = 1.0;
    let eps = 1e-5; // 収束判定条件

    while error > eps && it <= cnf.itmax {
        fm = capital_f(&cnf, p21, p1, p5, rho1, rho5, u1, u5);
        df = fm - fmm;
        p21 = p21 - (p21 - pmm) * fm / (df + 1.0e-8 * df / (f64::abs(df) + 1.0e-8));
        error = f64::abs(p21 - pm) / pm;
        pmm = pm;
        pm = p21;
        fmm = fm;

        it = it + 1;
    }

    if it > cnf.itmax {
        println!("反復最大回数を超えました.");
    }
    // 領域 2 の物理量を計算する
    let rho2 = rho1 * (p21 + (cnf.gamma - 1.0) / (cnf.gamma + 1.0))
        / ((cnf.gamma - 1.0) * p21 / (cnf.gamma + 1.0) + 1.0); // 密度
    let u2 = u1
        + c1 * f64::sqrt(2.0 / cnf.gamma) * (p21 - 1.0)
            / f64::sqrt(cnf.gamma - 1.0 + p21 * (cnf.gamma + 1.0)); // 流速
    let p2 = p21 * p1; // 圧力

    // 領域 3 の物理量を計算する
    let u3 = u2;
    let p3 = p2;
    let rho3 = rho5 * (p3 / p5).powf(1.0 / cnf.gamma);
    let c3 = f64::sqrt(cnf.gamma * p3 / rho3);

    // 各波の速度を計算する
    let vs = u1 + c1 * f64::sqrt((cnf.gamma + 1.0) / (2.0 * cnf.gamma) * (p21 - 1.0) + 1.0); // 衝撃波
    let vc = u3; // 接触不連続
    let vrt = u3 - c3; // 膨張波末端の速度
    let vrh = u5 - c5; // 膨張波先端の速度

    // t 時刻における波の位置を計算する
    let xs = x0 + vs * cdata.t; // 衝撃波
    let xc = x0 + vc * cdata.t; // 接触不連続
    let xrt = x0 + vrt * cdata.t; // 膨張波末端の速度
    let xrh = x0 + vrh * cdata.t; // 膨張波先端の速度

    // 計算格子に解を与える
    for i in 0..(cnf.node - 1) {
        // 領域 5
        if cdata.x[i] < xrh {
            rhoexact[i] = rho5;
            pexact[i] = p5;
            uexact[i] = u5;
        }
        // 領域 4
        else if cdata.x[i] <= xrt {
            uexact[i] = 2.0 / (cnf.gamma + 1.0)
                * (0.5 * (cnf.gamma - 1.0) * u5 + c5 + (cdata.x[i] - x0) / cdata.t);
            let c4 = c5 - 0.5 * (cnf.gamma - 1.0) * (uexact[i] - u5);
            pexact[i] = p5 * (c4 / c5).powf(2.0 * cnf.gamma / (cnf.gamma - 1.0));
            rhoexact[i] = rho5 * (cdata.pexact[i] / p5).powf(1.0 / cnf.gamma);
        }
        // 領域 3
        else if cdata.x[i] < xc {
            rhoexact[i] = rho3;
            pexact[i] = p3;
            uexact[i] = u3;
        }
        // 領域 2
        else if cdata.x[i] < xs {
            rhoexact[i] = rho2;
            pexact[i] = p2;
            uexact[i] = u2;
        }
        // 領域 1
        else {
            rhoexact[i] = rho1;
            pexact[i] = p1;
            uexact[i] = u1;
        }
    }
    ExactSolutions {
        rhoexact: rhoexact,
        pexact: pexact,
        uexact: uexact,
    }
}

fn main() {
    let mut cnf = Config::new().unwrap_or_else(|err| {
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
    let esols = exact_solution(&cnf, &cdata);
    cdata.rhoexact = esols.rhoexact;
    cdata.pexact = esols.pexact;
    cdata.uexact = esols.uexact;

    CalcData::write_csv(&cnf, &cdata)
        .map_err(|err| println!("{:?}", err))
        .ok();
    CalcData::write_png(&cnf, &cdata)
        .map_err(|err| println!("{:?}", err))
        .ok();

    while cdata.t <= cnf.tstop {
        cdata.n = cdata.n + 1;
        cdata.t = cdata.t + cnf.dt;

        if cdata.n % cnf.output_step == 0 {
            println!("Now {}/{} times!", &cdata.n, &cnf.nstop);
        }
        // cfl_check
        cnf.dt = cfl_check(&cnf, &cdata);

        // Lax_Wendroff
        for i in 0..(cnf.node - 2) {
            cdata.rho_star[i + 1] = 0.5 * (cdata.rho[i + 1] + cdata.rho[i])
                - 0.5 * cnf.dt * (cdata.rhou[i + 1] - cdata.rhou[i]) / cnf.dx;
            let p1 = (cnf.gamma - 1.0)
                * (cdata.rhoe[i + 1]
                    - 0.5 * cdata.rhou[i + 1] * cdata.rhou[i + 1] / cdata.rho[i + 1]);
            let p2 = (cnf.gamma - 1.0)
                * (cdata.rhoe[i] - 0.5 * cdata.rhou[i] * cdata.rhou[i] / cdata.rho[i]);
            cdata.rhou_star[i + 1] = 0.5 * (cdata.rhou[i + 1] + cdata.rhou[i])
                - 0.5
                    * cnf.dt
                    * ((cdata.rhou[i + 1] * cdata.rhou[i + 1] / cdata.rho[i + 1] + p1)
                        - (cdata.rhou[i] * cdata.rhou[i] / cdata.rho[i] + p2))
                    / cnf.dx;
            cdata.rhoe_star[i + 1] = 0.5 * (cdata.rhoe[i + 1] + cdata.rhoe[i])
                - 0.5
                    * cnf.dt
                    * ((cdata.rhou[i + 1] * ((cdata.rhoe[i + 1] + p1) / cdata.rho[i + 1]))
                        - (cdata.rhou[i] * ((cdata.rhoe[i] + p2) / cdata.rho[i])))
                    / cnf.dx;
        }

        for i in 0..(cnf.node - 2) {
            cdata.f1[i] = cdata.rhou_star[i];
            let p = (cnf.gamma - 1.0)
                * (cdata.rhoe_star[i]
                    - 0.5 * cdata.rhou_star[i] * cdata.rhou_star[i] / cdata.rho_star[i]);
            cdata.f2[i] = cdata.rhou_star[i] * cdata.rhou_star[i] / cdata.rho_star[i] + p;
            cdata.f3[i] = cdata.rhou_star[i] * (cdata.rhoe_star[i] + p) / cdata.rho_star[i];
        }

        // 左端における数値流束の境界条件
        cdata.f1[1] = cdata.f1[2];
        cdata.f2[1] = cdata.f2[2];
        cdata.f3[1] = cdata.f3[2];
        cdata.f1[0] = cdata.f1[1];
        cdata.f2[0] = cdata.f2[1];
        cdata.f3[0] = cdata.f3[1];
        // 右端における数値流束の境界条件
        cdata.f1[cnf.node - 1] = cdata.f1[cnf.node - 2];
        cdata.f2[cnf.node - 1] = cdata.f2[cnf.node - 2];
        cdata.f3[cnf.node - 1] = cdata.f3[cnf.node - 2];

        // update
        for i in 1..(cnf.node - 1) {
            // 時間積分 一次オイラー前進時間積分法による保存変数の更新
            cdata.rho[i] = cdata.rho[i]
                - cnf.dt * (cdata.f1[i + 1] - cdata.f1[i]) / (cdata.x[i + 1] - cdata.x[i]);
            cdata.rhou[i] = cdata.rhou[i]
                - cnf.dt * (cdata.f2[i + 1] - cdata.f2[i]) / (cdata.x[i + 1] - cdata.x[i]);
            cdata.rhoe[i] = cdata.rhoe[i]
                - cnf.dt * (cdata.f3[i + 1] - cdata.f3[i]) / (cdata.x[i + 1] - cdata.x[i]);
        }

        // exact
        let esols = exact_solution(&cnf, &cdata);
        cdata.rhoexact = esols.rhoexact;
        cdata.pexact = esols.pexact;
        cdata.uexact = esols.uexact;

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

fn cfl_check(cnf: &Config, cdata: &CalcData) -> f64 {
    let mut lambda_max: f64 = 0.0;
    let mut dt_min: f64 = 0.0;
    for i in 0..(cnf.node - 1) {
        let u = cdata.rhou[i] / cdata.rho[i];
        let p = (cnf.gamma - 1.0)
            * (cdata.rhoe[i] - 0.5 * cdata.rhou[i] * cdata.rhou[i] / cdata.rho[i]);
        let c = f64::sqrt(cnf.gamma * p / cdata.rho[i]);
        lambda_max = if f64::abs(lambda_max) > f64::abs(u + c) {
            lambda_max
        } else {
            u + c
        };
        lambda_max = if f64::abs(lambda_max) > f64::abs(u - c) {
            lambda_max
        } else {
            u - c
        };
        dt_min = cnf.cfl * cnf.dx / f64::max(f64::abs(lambda_max), 0.1);
    }
    dt_min
}

fn capital_f(
    cnf: &Config,
    p21: f64,
    p1: f64,
    p5: f64,
    rho1: f64,
    rho5: f64,
    u1: f64,
    u5: f64,
) -> f64 {
    let c1 = f64::sqrt(cnf.gamma * p1 / rho1);
    let c5 = f64::sqrt(cnf.gamma * p5 / rho5);
    let e1 = (((cnf.gamma + 1.0) / cnf.gamma * (p21 - 1.0)) / 2.0 + 1.0).powf(-0.5);
    let e2 = (1.0 + (cnf.gamma - 1.0) * (u5 - u1 - c1 / cnf.gamma * (p21 - 1.0) * e1) / c5 / 2.0)
        .powf(2.0 * cnf.gamma / (cnf.gamma - 1.0));
    let w = p5 * e2 / p1 - p21;
    w
}
