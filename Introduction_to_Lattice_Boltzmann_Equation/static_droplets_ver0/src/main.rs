// 格子ボルツマン法の本のサンプルコードの Rust 版, 静止液滴の方のコード
extern crate chrono;
extern crate ndarray;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate serde_yaml;
use chrono::Local;
use ndarray::array;
use ndarray::Array;
use ndarray::Array1;
use ndarray::Array2;
use ndarray::Array3;
use ndarray::Array4;
use ndarray::Array5;
use std::convert::TryInto;
//use serde::{Deserialize, Serialize};
//use std::env;
use std::error;
use std::fs;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufWriter;
//use std::process;
//use std::process::Command;

const PI: f64 = std::f64::consts::PI; // 円周率

fn write_config(cnf: &Config) -> Result<(), Box<dyn error::Error>> {
    let s = serde_yaml::to_string(&cnf)?;
    let file_name: String = format!("{}/params.yaml", cnf.dir_name);
    let file = File::create(file_name).unwrap();
    let mut w = BufWriter::new(file);
    write!(w, "{}", &s).unwrap();
    w.flush().unwrap();

    Ok(())
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct Config {
    dir_name: String,
    ds: f64,
    imax: usize,
    jmax: usize,
    kmax: usize,
    D: f64,
    h0: f64,
    xc: f64,
    yc: f64,
    zc: f64,
    ratio_rho: f64,
    ratio_mu: f64,
    rhog: f64,
    rhol: f64,
    mug: f64,
    mul: f64,
    sigma: f64,
    a: f64,
    b: f64,
    T: f64,
    phi1: f64,
    phi2: f64,
    kappaf: f64,
    C: f64,
    M: f64,
    itr_max: i64,
    omega_max: f64,
    lambda: f64,
    step: i64,
    start_sigma: i64,
    end_sigma: i64,
    epsilon: f64,
    rinput: f64,
    vinput: f64,
    dim: usize,
    fifteen: usize,
}

impl Config {
    fn new() -> Config {
        let dir_name = Local::now().format("workspace/%Y%m%d-%H%M%S").to_string();

        //====================LBM 用定数=====================================
        let ds: f64 = 1.0; // 格子間隔 (lattice unit)
        let imax: usize = 96; // 格子数 (x 方向)
        let jmax: usize = 96; // 格子数 (y 方向)
        let kmax: usize = 96; // 格子数 (z 方向)
        let D: f64 = 40.0 * ds; // 初期液滴直径
        let h0 = D; // 代表長さ

        // 初期液滴中心
        let xc: f64 = 0.5 * (imax as f64);
        let yc: f64 = 0.5 * (jmax as f64);
        let zc: f64 = 0.5 * (kmax as f64);

        //====================================================================
        //====================支配パラメタ====================================
        let ratio_rho: f64 = 800.0; // 密度比

        let ratio_mu: f64 = 50.0; // 粘度比
        let rhog: f64 = 1.0; // 気相密度
        let rhol: f64 = ratio_rho * rhog; // 液相密度

        let mug: f64 = 1.6e-2 * ds; // 気相粘度
        let mul: f64 = ratio_mu * mug; // 液相粘度

        let sigma: f64 = 1.2e-2 * ds; // 界面張力

        //====================================================================
        //=====================phi 計算用定数==================================
        let a: f64 = 1.0;
        let b: f64 = 1.0;
        let T: f64 = 2.93e-1;
        let phi1: f64 = 2.638e-1;
        let phi2: f64 = 4.031e-1;

        let kappaf: f64 = 0.06 * ds.powf(2.0);
        let C = 0.0;
        let M = (0.5 - C / 3.0) * ds; // モビリティ

        //====================================================================
        //=====================圧力・流速計算用定数===========================
        let itr_max: i64 = 2; // 反復回数
        let omega_max: f64 = rhol / (itr_max as f64); // 加速パラメタの最大値
        let lambda: f64 = 1.0; // 安定項の係数

        //====================================================================
        //=====================その他定数・変数===============================
        let step: i64 = 20000; //時間ステップ上限
        let start_sigma: i64 = 5000; // 界面張力をかけ始めるステップ数
        let end_sigma: i64 = 15000; // 界面張力をかけ終わるステップ数
        let epsilon: f64 = 1.0e-12; // 零除算を防ぐための閾値

        let rinput: f64 = 0.5 * D; // インプット半径
        let vinput: f64 = 4.0 / 3.0 * PI * rinput.powf(3.0); // インプット体積

        Config {
            dir_name: dir_name,
            ds: ds,
            imax: imax,
            jmax: jmax,
            kmax: kmax,
            D: D,
            h0: h0,
            xc: xc,
            yc: yc,
            zc: zc,
            ratio_rho: ratio_rho,
            ratio_mu: ratio_mu,
            rhog: rhog,
            rhol: rhol,
            mug: mug,
            mul: mul,
            sigma: sigma,
            a: a,
            b: b,
            T: T,
            phi1: phi1,
            phi2: phi2,
            kappaf: kappaf,
            C: C,
            M: M,
            itr_max: itr_max,
            omega_max: omega_max,
            lambda: lambda,
            step: step,
            start_sigma: start_sigma,
            end_sigma: end_sigma,
            epsilon: epsilon,
            rinput: rinput,
            vinput: vinput,
            dim: 3,
            fifteen: 15,
        }
    }
}

struct CalcData {
    ci: Array1<i64>,       // 粒子速度 (整数)
    cj: Array1<i64>,       // 粒子速度 (整数)
    ck: Array1<i64>,       // 粒子速度 (整数)
    cr: Array2<f64>,       // 粒子速度 (実数)
    E: Array1<f64>,        // 係数
    H: Array1<f64>,        // 係数
    F: Array1<f64>,        // 係数
    feq: Array4<f64>,      // 平衡分布関数
    phi: Array3<f64>,      // index function
    lap_phi: Array3<f64>,  // index function計算用中間変数
    grad_phi: Array4<f64>, // index function計算用中間変数
    p0: Array3<f64>,       // index function計算用中間変数
    gphi: Array5<f64>,     // index function計算用中間変数
    pcap: Array5<f64>,     // index function計算用中間変数
    div_pcap: Array4<f64>, // index function計算用中間変数
    phi_min: f64,          // phi の最大値
    phi_max: f64,          // phi の最小値
    geq: Array4<f64>,      // 平衡分布関数
    rho: Array3<f64>,      // 密度
    p: Array3<f64>,        // 圧力
    u: Array3<f64>,        // 流速
    v: Array3<f64>,        // 流速
    w: Array3<f64>,        // 流速
    omega: Array3<f64>,    // 加速パラメタ
    delta_p: Array4<f64>,  // 圧力計算時の中間変数
    rhonext: Array3<f64>,  // 次ステップの密度
    unext: Array3<f64>,    // 次ステップの流速
    vnext: Array3<f64>,    // 次ステップの流速
    wnext: Array3<f64>,    // 次ステップの流速
    mu: Array3<f64>,       // 粘度
    Au: Array3<f64>,       // 粘度に関する変数
    grad_mu: Array4<f64>,  // 粘度の勾配
    grad_u: Array4<f64>,   // 流速勾配
    vcap: Array4<f64>,     // V_alpha
    lap_u: Array3<f64>,    // u のラプラシアン
    lap_v: Array3<f64>,    // v のラプラシアン
    lap_w: Array3<f64>,    // w のラプラシアン
    laplap_u: Array3<f64>, // lap_u のラプラシアン
    laplap_v: Array3<f64>, // lap_v のラプラシアン
    laplap_w: Array3<f64>, // lap_w のラプラシアン
    grad_rho: Array4<f64>, // 密度の勾配
    normal: Array4<f64>,   // 法線方向
    chi: Array3<f64>,      // 曲率
    fsv: Array4<f64>,      // 界面張力
    sigma_temp: f64,       // 各ステップにおける界面張力の値
    krone: Array2<f64>,    // クロネッカーのデルタ
    gtemp: f64,            // 一時的な変数
    veff: f64,             // 実効液滴体積
    reff: f64,             // 実効液滴半径
    dp_th: f64,            // Laplace 圧の理論値
    pg: f64,               // ガス圧
    pl: f64,               // 液圧
    dp_calc: f64,          // 気液圧力差の計算値
    err: f64,              // Laplace 圧からの誤差
}

impl CalcData {
    fn new(cnf: &Config) -> CalcData {
        let ci: Array1<i64> = array![0, 1, 0, 0, -1, 0, 0, 1, -1, 1, 1, -1, 1, -1, -1,];
        let cj: Array1<i64> = array![0, 0, 1, 0, 0, -1, 0, 1, 1, -1, 1, -1, -1, 1, -1,];
        let ck: Array1<i64> = array![0, 0, 0, 1, 0, 0, -1, 1, 1, 1, -1, -1, -1, -1, 1,];

        let mut cr: Array2<f64> = Array2::zeros((cnf.dim, cnf.fifteen));
        // 値の代入
        for l in 0..cnf.fifteen {
            cr[(0, l)] = ci[l] as f64;
            cr[(1, l)] = cj[l] as f64;
            cr[(2, l)] = ck[l] as f64;
        }

        let E: Array1<f64> = array![
            2.0 / 9.0,
            1.0 / 9.0,
            1.0 / 9.0,
            1.0 / 9.0,
            1.0 / 9.0,
            1.0 / 9.0,
            1.0 / 9.0,
            1.0 / 72.0,
            1.0 / 72.0,
            1.0 / 72.0,
            1.0 / 72.0,
            1.0 / 72.0,
            1.0 / 72.0,
            1.0 / 72.0,
            1.0 / 72.0,
        ];
        let H: Array1<f64> =
            array![1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,];
        let F: Array1<f64> = array![
            -7.0 / 3.0,
            1.0 / 3.0,
            1.0 / 3.0,
            1.0 / 3.0,
            1.0 / 3.0,
            1.0 / 3.0,
            1.0 / 3.0,
            1.0 / 24.0,
            1.0 / 24.0,
            1.0 / 24.0,
            1.0 / 24.0,
            1.0 / 24.0,
            1.0 / 24.0,
            1.0 / 24.0,
            1.0 / 24.0,
        ];

        //====================================================================
        //======================phi 計算用変数=================================
        let feq: Array4<f64> = Array::zeros((cnf.fifteen, cnf.imax, cnf.jmax, cnf.kmax));
        let phi: Array3<f64> = Array::zeros((cnf.imax, cnf.jmax, cnf.kmax));

        // index function 計算用中間変数
        let lap_phi: Array3<f64> = Array::zeros((cnf.imax, cnf.jmax, cnf.kmax));
        let grad_phi: Array4<f64> = Array::zeros((cnf.dim, cnf.imax, cnf.jmax, cnf.kmax));
        let p0: Array3<f64> = Array::zeros((cnf.imax, cnf.jmax, cnf.kmax));
        let gphi: Array5<f64> = Array::zeros((cnf.dim, cnf.dim, cnf.imax, cnf.jmax, cnf.kmax));
        let pcap: Array5<f64> = Array::zeros((cnf.dim, cnf.dim, cnf.imax, cnf.jmax, cnf.kmax));
        let div_pcap: Array4<f64> = Array::zeros((cnf.dim, cnf.imax, cnf.jmax, cnf.kmax));
        let phi_min: f64 = 0.0;
        let phi_max: f64 = 0.0;

        //====================================================================
        //======================圧力・流速計算用変数==========================
        let geq: Array4<f64> = Array::zeros((cnf.dim, cnf.imax, cnf.jmax, cnf.kmax));
        let rho: Array3<f64> = Array::zeros((cnf.imax, cnf.jmax, cnf.kmax));
        let p: Array3<f64> = Array::from_elem((cnf.imax, cnf.jmax, cnf.kmax), 1.0 / 3.0);
        let u: Array3<f64> = Array::zeros((cnf.imax, cnf.jmax, cnf.kmax));
        let v: Array3<f64> = Array::zeros((cnf.imax, cnf.jmax, cnf.kmax));
        let w: Array3<f64> = Array::zeros((cnf.imax, cnf.jmax, cnf.kmax));
        let omega: Array3<f64> = Array::zeros((cnf.imax, cnf.jmax, cnf.kmax));
        let delta_p: Array4<f64> = Array::zeros((cnf.fifteen, cnf.imax, cnf.jmax, cnf.kmax));
        let rhonext: Array3<f64> = Array::zeros((cnf.imax, cnf.jmax, cnf.kmax));
        let unext: Array3<f64> = Array::zeros((cnf.imax, cnf.jmax, cnf.kmax));
        let vnext: Array3<f64> = Array::zeros((cnf.imax, cnf.jmax, cnf.kmax));
        let wnext: Array3<f64> = Array::zeros((cnf.imax, cnf.jmax, cnf.kmax));

        let mu: Array3<f64> = Array::zeros((cnf.imax, cnf.jmax, cnf.kmax));
        let Au: Array3<f64> = Array::zeros((cnf.imax, cnf.jmax, cnf.kmax));
        let grad_mu: Array4<f64> = Array::zeros((cnf.dim, cnf.imax, cnf.jmax, cnf.kmax));
        let grad_u: Array4<f64> = Array::zeros((cnf.dim, cnf.imax, cnf.jmax, cnf.kmax));
        let vcap: Array4<f64> = Array::zeros((cnf.dim, cnf.imax, cnf.jmax, cnf.kmax));

        let lap_u: Array3<f64> = Array::zeros((cnf.imax, cnf.jmax, cnf.kmax));
        let lap_v: Array3<f64> = Array::zeros((cnf.imax, cnf.jmax, cnf.kmax));
        let lap_w: Array3<f64> = Array::zeros((cnf.imax, cnf.jmax, cnf.kmax));
        let laplap_u: Array3<f64> = Array::zeros((cnf.imax, cnf.jmax, cnf.kmax));
        let laplap_v: Array3<f64> = Array::zeros((cnf.imax, cnf.jmax, cnf.kmax));
        let laplap_w: Array3<f64> = Array::zeros((cnf.imax, cnf.jmax, cnf.kmax));

        let grad_rho: Array4<f64> = Array::zeros((cnf.dim, cnf.imax, cnf.jmax, cnf.kmax));
        let normal: Array4<f64> = Array::zeros((cnf.dim, cnf.imax, cnf.jmax, cnf.kmax));
        let chi: Array3<f64> = Array::zeros((cnf.imax, cnf.jmax, cnf.kmax));
        let fsv: Array4<f64> = Array::zeros((cnf.dim, cnf.imax, cnf.jmax, cnf.kmax));

        let sigma_temp: f64 = 0.0;

        let krone: Array2<f64> = Array2::eye(cnf.dim);

        let gtemp: f64 = 0.0;

        let veff: f64 = 0.0;
        let reff: f64 = 0.0;
        let dp_th: f64 = 0.0;
        let pg: f64 = 0.0;
        let pl: f64 = 0.0;
        let dp_calc: f64 = 0.0;
        let err: f64 = 0.0;

        CalcData {
            ci: ci,
            cj: cj,
            ck: ck,
            cr: cr,
            E: E,
            H: H,
            F: F,
            feq: feq,
            phi: phi,
            lap_phi: lap_phi,
            grad_phi: grad_phi,
            p0: p0,
            gphi: gphi,
            pcap: pcap,
            div_pcap: div_pcap,
            phi_min: phi_min,
            phi_max: phi_max,
            geq: geq,
            rho: rho,
            p: p,
            u: u,
            v: v,
            w: w,
            omega: omega,
            delta_p: delta_p,
            rhonext: rhonext,
            unext: unext,
            vnext: vnext,
            wnext: wnext,
            mu: mu,
            Au: Au,
            grad_mu: grad_mu,
            grad_u: grad_u,
            vcap: vcap,
            lap_u: lap_u,
            lap_v: lap_v,
            lap_w: lap_w,
            laplap_u: laplap_u,
            laplap_v: laplap_v,
            laplap_w: laplap_w,
            grad_rho: grad_rho,
            normal: normal,
            chi: chi,
            fsv: fsv,
            sigma_temp: sigma_temp,
            krone: krone,
            gtemp: gtemp,
            veff: veff,
            reff: reff,
            dp_th: dp_th,
            pg: pg,
            pl: pl,
            dp_calc: dp_calc,
            err: err,
        }
    }
}

fn main() {
    let cnf: Config = Config::new();

    // 出力用ディレクトリがなければ作る
    fs::create_dir_all(&cnf.dir_name).unwrap_or_else(|why| {
        println!("! {:?}", why.kind());
    });

    let mut cdata: CalcData = CalcData::new(&cnf);

    write_config(&cnf).map_err(|err| println!("{:?}", err)).ok();

    //======================初期条件======================================
    for k in 0..cnf.kmax {
        for j in 0..cnf.jmax {
            for i in 0..cnf.imax {
                let x: f64 = (i as f64) * cnf.ds - cnf.xc;
                let y: f64 = (j as f64) * cnf.ds - cnf.yc;
                let z: f64 = (k as f64) * cnf.ds - cnf.zc;
                let r: f64 = 0.5 * cnf.D;
                if x.powf(2.0) + y.powf(2.0) + z.powf(2.0) <= r.powf(2.0) {
                    cdata.phi[(i, j, k)] = cnf.phi2;
                }

                // 密度の関数形：線形補間，三角関数のどちらかを選択
                // cata.rho[(i,j,k)] = (phi(i,j,k)-phi1)/(phi2-phi1)*(rhol-rhog) + rhog
                cdata.rho[(i, j, k)] = 0.5
                    * (cnf.rhol - cnf.rhog)
                    * (f64::sin(
                        PI * (cdata.phi[(i, j, k)] - 0.5 * (cnf.phi2 + cnf.phi1))
                            / (cnf.phi2 - cnf.phi1),
                    ) + 1.0)
                    + cnf.rhog;
            }
        }
    }

    //======================時間発展======================================
    for n in 1..cnf.step {
        for k in 0..cnf.kmax {
            for j in 0..cnf.jmax {
                for i in 0..cnf.imax {
                    //----------------------------------------------------------phiの計算
                    //-----------lap_phi の計算（全面周期条件）-------
                    cdata.lap_phi[(i, j, k)] = -14.0 * cdata.phi[(i, j, k)];

                    if is_in_bulk(i, j, k, &cnf) {
                        // bulk 部分
                        for l in 1..(cnf.fifteen - 1) {
                            cdata.lap_phi[(i, j, k)] = cdata.lap_phi[(i, j, k)]
                                + cdata.phi[(
                                    ((i as i64) + cdata.ci[l]) as usize,
                                    ((j as i64) + cdata.cj[l]) as usize,
                                    ((k as i64) + cdata.ck[l]) as usize,
                                )];
                        }
                    } else {
                        // 境界 (周期条件)
                        for l in 1..(cnf.fifteen - 1) {
                            cdata.lap_phi[(i, j, k)] = cdata.lap_phi[(i, j, k)]
                                + cdata.phi[(
                                    perx((i as i64) + cdata.ci[(l)], &cnf),
                                    pery((j as i64) + cdata.cj[(l)], &cnf),
                                    perz((k as i64) + cdata.ck[(l)], &cnf),
                                )];
                        }
                    }
                    cdata.lap_phi[(i, j, k)] = cdata.lap_phi[(i, j, k)] / 5.0 / cnf.ds.powf(2.0);

                    //-----------------------------------------------
                    //-----------grad_phi の計算（全面周期）----------
                    for alpha in 0..(cnf.dim - 1) {
                        cdata.grad_phi[(alpha, i, j, k)] = 0.0;

                        // bulk 部分
                        if is_in_bulk(i, j, k, &cnf) {
                            for l in 1..(cnf.fifteen - 1) {
                                cdata.grad_phi[(alpha, i, j, k)] = cdata.grad_phi[(alpha, i, j, k)]
                                    + cdata.cr[(alpha, l)]
                                        * cdata.phi[(
                                            ((i as i64) + cdata.ci[(l)]) as usize,
                                            ((j as i64) + cdata.cj[(l)]) as usize,
                                            ((k as i64) + cdata.ck[(l)]) as usize,
                                        )];
                            }
                        } else {
                            // 境界（周期条件）
                            for l in 1..(cnf.fifteen - 1) {
                                cdata.grad_phi[(alpha, i, j, k)] = cdata.grad_phi[(alpha, i, j, k)]
                                    + cdata.cr[(alpha, l)]
                                        * cdata.phi[(
                                            perx((i as i64) + cdata.ci[(l)], &cnf),
                                            pery((j as i64) + cdata.cj[(l)], &cnf),
                                            perz((k as i64) + cdata.ck[(l)], &cnf),
                                        )];
                            }
                        }
                        cdata.grad_phi[(alpha, i, j, k)] =
                            cdata.grad_phi[(alpha, i, j, k)] / (10.0 * cnf.ds);
                    }
                    // -----------------------------------------------
                    // -----------p0 の計算----------------------------
                    cdata.p0[(i, j, k)] = cdata.phi[(i, j, k)] * cnf.T
                        / (1.0 - cnf.b * cdata.phi[(i, j, k)])
                        - cnf.a * cdata.phi[(i, j, k)].powf(2.0);

                    // -----------------------------------------------
                    // -----------gphi の計算--------------------------
                    //-----------------------------------------------
                    //-----------pcap の計算--------------------------
                    for beta in 0..(cnf.dim - 1) {
                        for alpha in 0..(cnf.dim - 1) {
                            cdata.gphi[(alpha, beta, i, j, k)] = 4.5
                                * cdata.gphi[(alpha, beta, i, j, k)]
                                * cdata.grad_phi[(beta, i, j, k)]
                                - 1.5
                                    * (cdata.grad_phi[(0, i, j, k)].powf(2.0)
                                        + cdata.grad_phi[(1, i, j, k)].powf(2.0)
                                        + cdata.grad_phi[(2, i, j, k)].powf(2.0))
                                    * cdata.krone[(alpha, beta)];

                            cdata.pcap[(alpha, beta, i, j, k)] = (cdata.p0[(i, j, k)]
                                - cnf.kappaf * cdata.phi[(i, j, k)] * cdata.lap_phi[(i, j, k)]
                                - 0.5
                                    * cnf.kappaf
                                    * (cdata.grad_phi[(0, i, j, k)].powf(2.0)
                                        + cdata.grad_phi[(1, i, j, k)].powf(2.0)
                                        + cdata.grad_phi[(2, i, j, k)].powf(2.0)))
                                * cdata.krone[(alpha, beta)]
                                + cnf.kappaf
                                    * cdata.grad_phi[(alpha, i, j, k)]
                                    * cdata.grad_phi[(beta, i, j, k)];
                        }
                    }
                    //-----------------------------------------------
                    //------div_pcap の計算（全面周期条件）-----------
                    for alpha in 0..(cnf.dim - 1) {
                        cdata.div_pcap[(alpha, i, j, k)] = 0.0;
                        // bulk 部分
                        if is_in_bulk(i, j, k, &cnf) {
                            for l in 1..(cnf.fifteen - 1) {
                                for beta in 0..(cnf.dim - 1) {
                                    cdata.div_pcap[(alpha, i, j, k)] = cdata.div_pcap
                                        [(alpha, i, j, k)]
                                        + cdata.cr[(beta, l)]
                                            * cdata.pcap[(
                                                alpha,
                                                beta,
                                                i + cdata.ci[(l)] as usize,
                                                j + cdata.cj[(l)] as usize,
                                                k + cdata.ck[(l)] as usize,
                                            )];
                                }
                            }
                        } else {
                            // 境界（周期条件）
                            for l in 1..(cnf.fifteen - 1) {
                                for beta in 0..(cnf.dim - 1) {
                                    cdata.div_pcap[(alpha, i, j, k)] = cdata.div_pcap
                                        [(alpha, i, j, k)]
                                        + cdata.cr[(beta, l)]
                                            * cdata.pcap[(
                                                alpha,
                                                beta,
                                                perx((i as i64) + cdata.ci[(l)], &cnf),
                                                perx((j as i64) + cdata.cj[(l)], &cnf),
                                                perx((k as i64) + cdata.ck[(l)], &cnf),
                                            )];
                                }
                            }
                        }
                        cdata.div_pcap[(alpha, i, j, k)] =
                            cdata.div_pcap[(alpha, i, j, k)] / (10.0 * cnf.ds);
                    }
                    // -----------------------------------------------
                    // -----------feq の計算---------------------------
                    for l in 0..(cnf.fifteen - 1) {
                        let mut gtemp = 0.0;
                        for beta in 0..(cnf.dim - 1) {
                            for alpha in 0..(cnf.dim - 1) {
                                gtemp = gtemp
                                    + cdata.gphi[(alpha, beta, i, j, k)]
                                        * cdata.gphi[(alpha, beta, i, j, k)]
                                        * cdata.cr[(alpha, l)]
                                        * cdata.cr[(beta, l)];
                            }
                        }
                        cdata.feq[(l, i, j, k)] = cdata.H[(l)] * cdata.phi[(i, j, k)]
                            + cdata.F[(l)]
                                * (cdata.p0[(i, j, k)]
                                    - cnf.kappaf * cdata.phi[(i, j, k)] * cdata.lap_phi[(i, j, k)]
                                    - cnf.kappaf / 6.0
                                        * (cdata.grad_phi[(0, i, j, k)].powf(2.0)
                                            + cdata.grad_phi[(1, i, j, k)].powf(2.0)
                                            + cdata.grad_phi[(2, i, j, k)].powf(2.0)))
                            + 3.0
                                * cdata.E[(l)]
                                * cdata.phi[(i, j, k)]
                                * (cdata.cr[(0, l)] * cdata.u[(i, j, k)]
                                    + cdata.cr[(1, l)] * cdata.v[(i, j, k)]
                                    + cdata.cr[(2, l)] * cdata.w[(i, j, k)])
                            + cdata.E[(l)] * cnf.kappaf * gtemp
                            + cdata.E[(l)]
                                * cnf.C
                                * (cdata.div_pcap[(0, i, j, k)] * cdata.cr[(0, l)]
                                    + cdata.div_pcap[(1, i, j, k)] * cdata.cr[(1, l)]
                                    + cdata.div_pcap[(2, i, j, k)] * cdata.cr[(2, l)])
                                * cnf.ds;
                    }
                    // -----------------------------------------------
                    // ***********phi の時間発展（全面周期）***********
                    cdata.phi[(i, j, k)] = 0.0;
                    // bulk部分
                    if is_in_bulk(i, j, k, &cnf) {
                        for l in 0..(cnf.fifteen - 1) {
                            cdata.phi[(i, j, k)] = cdata.phi[(i, j, k)]
                                + cdata.feq[(
                                    l,
                                    ((i as i64) - cdata.ci[(l)]) as usize,
                                    ((j as i64) - cdata.cj[(l)]) as usize,
                                    ((k as i64) - cdata.ck[(l)]) as usize,
                                )];
                        }
                    } else {
                        for l in 0..(cnf.fifteen - 1) {
                            cdata.phi[(i, j, k)] = cdata.phi[(i, j, k)]
                                + cdata.feq[(
                                    l,
                                    perx((i as i64) - cdata.ci[(l)], &cnf),
                                    pery((j as i64) - cdata.cj[(l)], &cnf),
                                    perz((k as i64) - cdata.ck[(l)], &cnf),
                                )];
                        }
                    }
                } // i,,j, k ループ終了
            }
        }
    }
}

fn is_in_bulk(i: usize, j: usize, k: usize, cnf: &Config) -> bool {
    if i != 0 && i != cnf.imax && j != 0 && j != cnf.jmax && k != 0 && k != cnf.kmax {
        true
    } else {
        false
    }
}

fn perx(i: i64, cnf: &Config) -> usize {
    let imax = cnf.imax as i64;
    if i < 0 {
        (i + imax).try_into().unwrap()
    } else if i >= imax {
        (i - imax).try_into().unwrap()
    } else {
        i.try_into().unwrap()
    }
}

fn pery(j: i64, cnf: &Config) -> usize {
    let jmax = cnf.jmax as i64;
    if j < 0 {
        (j + jmax).try_into().unwrap()
    } else if j >= jmax {
        (j - jmax).try_into().unwrap()
    } else {
        j.try_into().unwrap()
    }
}

fn perz(k: i64, cnf: &Config) -> usize {
    let kmax = cnf.kmax as i64;
    if k < 0 {
        (k + kmax).try_into().unwrap()
    } else if k >= kmax {
        (k - kmax).try_into().unwrap()
    } else {
        k.try_into().unwrap()
    }
}
