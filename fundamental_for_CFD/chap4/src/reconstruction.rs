use crate::config::Config;
use crate::limiter::Limiter;
pub enum Reconstruction {
    PC,
    LaxWendorff,
    BeamWarming,
    Fromm,
    TVD,
}

pub struct ULR {
    pub uls: Vec<f64>,
    pub urs: Vec<f64>,
}

impl Reconstruction {
    pub fn new(reconstruction_str: &str) -> Reconstruction {
        match reconstruction_str {
            "PC" => Reconstruction::PC,
            "LaxWendorff" => Reconstruction::LaxWendorff,
            "BeamWarming" => Reconstruction::BeamWarming,
            "Fromm" => Reconstruction::Fromm,
            "TVD" => Reconstruction::TVD,
            _ => Reconstruction::PC, // TODO バリデート: 対象外ならエラーで即終了にしたい
        }
    }

    pub fn reconstruction(cnf: &Config, us: &Vec<f64>) -> ULR {
        match cnf.reconstruction {
            Reconstruction::PC => pc(&cnf, &us),
            Reconstruction::LaxWendorff => lax_wendorff(&cnf, &us),
            Reconstruction::BeamWarming => beam_waming(&cnf, &us),
            Reconstruction::Fromm => fromm(&cnf, &us),
            Reconstruction::TVD => match cnf.limiter {
                Limiter::Minmod => tvd_minmod(&cnf, &us),
                Limiter::Superbee => tvd_superbee(&cnf, &us),
                Limiter::VanLeer => tvd_vanleer(&cnf, &us),
                Limiter::VanAlbada => tvd_vanalbada(&cnf, &us),
                Limiter::FluxLimiter => tvd_flux_limiter(&cnf),
            },
        }
    }
}

fn pc(cnf: &Config, us: &Vec<f64>) -> ULR {
    let mut uls = vec![0.0; cnf.node];
    let mut urs = vec![0.0; cnf.node];

    for i in 1..(cnf.node - 2) {
        uls[i + 1] = us[i];
        urs[i + 1] = us[i + 1];
    }

    ULR { uls: uls, urs: urs }
}

fn lax_wendorff(cnf: &Config, us: &Vec<f64>) -> ULR {
    let mut uls = vec![0.0; cnf.node];
    let mut urs = vec![0.0; cnf.node];

    for i in 1..(cnf.node - 2) {
        let delta_l = 0.5 * (us[i + 1] - us[i]);
        let delta_r = 0.5 * (us[i + 1] - us[i]);
        // セル境界 (i+1/2) 左側の値
        uls[i + 1] = us[i] + (1.0 - cnf.alpha_12 * cnf.dt / cnf.dx) * delta_l;
        // セル境界 (i+1/2) 右側の値
        urs[i + 1] = us[i + 1] - (1.0 + cnf.alpha_12 * cnf.dt / cnf.dx) * delta_r;
    }

    ULR { uls: uls, urs: urs }
}

fn beam_waming(cnf: &Config, us: &Vec<f64>) -> ULR {
    let mut uls = vec![0.0; cnf.node];
    let mut urs = vec![0.0; cnf.node];

    for i in 1..(cnf.node - 2) {
        let delta_l = 0.5 * (us[i] - us[i - 1]);
        let delta_r = 0.5 * (us[i + 2] - us[i + 1]);
        // セル境界 (i+1/2) 左側の値
        uls[i + 1] = us[i] + (1.0 - cnf.alpha_12 * cnf.dt / cnf.dx) * delta_l;
        // セル境界 (i+1/2) 右側の値
        urs[i + 1] = us[i + 1] - (1.0 + cnf.alpha_12 * cnf.dt / cnf.dx) * delta_r;
    }

    ULR { uls: uls, urs: urs }
}

fn fromm(cnf: &Config, us: &Vec<f64>) -> ULR {
    let mut uls = vec![0.0; cnf.node];
    let mut urs = vec![0.0; cnf.node];

    for i in 1..(cnf.node - 2) {
        let delta_l = 0.25 * (us[i + 1] - us[i - 1]);
        let delta_r = 0.25 * (us[i + 2] - us[i]);
        // セル境界 (i+1/2) 左側の値
        uls[i + 1] = us[i] + (1.0 - cnf.alpha_12 * cnf.dt / cnf.dx) * delta_l;
        // セル境界 (i+1/2) 右側の値
        urs[i + 1] = us[i + 1] - (1.0 + cnf.alpha_12 * cnf.dt / cnf.dx) * delta_r;
    }

    ULR { uls: uls, urs: urs }
}

fn tvd_minmod(cnf: &Config, us: &Vec<f64>) -> ULR {
    let mut uls = vec![0.0; cnf.node];
    let mut urs = vec![0.0; cnf.node];

    for i in 1..(cnf.node - 2) {
        // 0 割り算を避けるための正則化
        let reg_term = sign(us[i + 1] - us[1]) * 1.0e-5;
        let r_l = (us[i] - us[i - 1]) / (us[i + 1] - us[i] + reg_term);
        let r_r = (us[i + 2] - us[i + 1]) / (us[i + 1] - us[i] + reg_term);
        let phi_l = minmod(r_l);
        let phi_r = minmod(r_r);
        let delta_l = 0.5 * (us[i + 1] - us[i]);
        let delta_r = 0.5 * (us[i + 1] - us[i]);
        // セル境界 (i+1/2) 左側の値
        uls[i + 1] = us[i] + (1.0 - cnf.alpha_12 * cnf.dt / cnf.dx) * phi_l * delta_l;
        // セル境界 (i+1/2) 右側の値
        urs[i + 1] = us[i + 1] - (1.0 + cnf.alpha_12 * cnf.dt / cnf.dx) * phi_r * delta_r;
    }
    ULR { uls: uls, urs: urs }
}

fn tvd_superbee(cnf: &Config, us: &Vec<f64>) -> ULR {
    let mut uls = vec![0.0; cnf.node];
    let mut urs = vec![0.0; cnf.node];

    for i in 1..(cnf.node - 2) {
        // 0 割り算を避けるための正則化
        let reg_term = sign(us[i + 1] - us[1]) * 1.0e-5;
        let r_l = (us[i] - us[i - 1]) / (us[i + 1] - us[i] + reg_term);
        let r_r = (us[i + 1] - us[i]) / (us[i + 1] - us[i] + reg_term);
        let phi_l = superbee(r_l);
        let phi_r = superbee(r_r);
        let delta_l = 0.5 * (us[i + 1] - us[i]);
        let delta_r = 0.5 * (us[i + 1] - us[i]);
        // セル境界 (i+1/2) 左側の値
        uls[i + 1] = us[i] + (1.0 - cnf.alpha_12 * cnf.dt / cnf.dx) * phi_l * delta_l;
        // セル境界 (i+1/2) 右側の値
        urs[i + 1] = us[i + 1] - (1.0 + cnf.alpha_12 * cnf.dt / cnf.dx) * phi_r * delta_r;
    }
    ULR { uls: uls, urs: urs }
}

fn tvd_vanleer(cnf: &Config, us: &Vec<f64>) -> ULR {
    let mut uls = vec![0.0; cnf.node];
    let mut urs = vec![0.0; cnf.node];

    for i in 1..(cnf.node - 2) {
        // 0 割り算を避けるための正則化
        let reg_term = sign(us[i + 1] - us[1]) * 1.0e-5;
        let r_l = (us[i] - us[i - 1]) / (us[i + 1] - us[i] + reg_term);
        let r_r = (us[i + 2] - us[i + 1]) / (us[i + 1] - us[i] + reg_term);
        let phi_l = van_leer(r_l);
        let phi_r = van_leer(r_r);
        let delta_l = 0.5 * (us[i + 1] - us[i]);
        let delta_r = 0.5 * (us[i + 1] - us[i]);
        uls[i + 1] = us[i] + (1.0 - cnf.alpha_12 * cnf.dt / cnf.dx) * phi_l * delta_l;
        urs[i + 1] = us[i + 1] + (1.0 - cnf.alpha_12 * cnf.dt / cnf.dx) * phi_r * delta_r;
    }
    ULR { uls: uls, urs: urs }
}

fn tvd_vanalbada(cnf: &Config, us: &Vec<f64>) -> ULR {
    let mut uls = vec![0.0; cnf.node];
    let mut urs = vec![0.0; cnf.node];

    for i in 1..(cnf.node - 2) {
        // 0 割り算を避けるための正則化
        let reg_term = sign(us[i + 1] - us[1]) * 1.0e-5;
        let r_l = (us[i] - us[i - 1]) / (us[i + 1] - us[i] + reg_term);
        let r_r = (us[i + 2] - us[i + 1]) / (us[i + 1] - us[i] + reg_term);
        let phi_l = van_albada(r_l);
        let phi_r = van_albada(r_r);
        let delta_l = 0.5 * (us[i + 1] - us[i]);
        let delta_r = 0.5 * (us[i + 1] - us[i]);
        // セル境界 (i+1/2) 左側の値
        uls[i + 1] = us[i] + (1.0 - cnf.alpha_12 * cnf.dt / cnf.dx) * phi_l * delta_l;
        // セル境界 (i+1/2) 右側の値
        urs[i + 1] = us[i + 1] - (1.0 + cnf.alpha_12 * cnf.dt / cnf.dx) * phi_r * delta_r;
    }
    ULR { uls: uls, urs: urs }
}

fn tvd_flux_limiter(cnf: &Config) -> ULR {
    ULR {
        uls: vec![0.0; cnf.node],
        urs: vec![0.0; cnf.node],
    }
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

pub fn sign(x: f64) -> f64 {
    if x >= 0.0 {
        1.0
    } else {
        -1.0
    }
}
