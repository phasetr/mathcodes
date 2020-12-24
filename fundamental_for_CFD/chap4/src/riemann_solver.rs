use crate::config::Config;
use crate::reconstruction::sign;

pub enum Riemann {
    Roe,
    FluxLimiter,
}

impl Riemann {
    pub fn new(riemann_str: &str) -> Riemann {
        match riemann_str {
            "roe" => Riemann::Roe,
            "fluxLimiter" => Riemann::FluxLimiter,
            _ => Riemann::Roe, // TODO バリデート: 対象外ならエラーで即終了にしたい
        }
    }

    pub fn riemann(cnf: &Config, us: &Vec<f64>, uls: &Vec<f64>, urs: &Vec<f64>) -> Vec<f64> {
        match cnf.riemann {
            Riemann::Roe => roe(&cnf, &uls, &urs),
            Riemann::FluxLimiter => flux_limiter(&cnf, &us),
        }
    }
}

fn roe(cnf: &Config, uls: &Vec<f64>, urs: &Vec<f64>) -> Vec<f64> {
    let mut fs = vec![0.0; cnf.node];
    for i in 2..(cnf.node - 1) {
        fs[i] = 1.0 / 2.0 * (cnf.a * uls[i] + cnf.a * urs[i])
            - 1.0 / 2.0 * f64::abs(cnf.a) * (urs[i] - uls[i]);
    }
    fs
}

fn flux_limiter(cnf: &Config, us: &Vec<f64>) -> Vec<f64> {
    let mut us_star = vec![0.0; cnf.node];
    let mut fs = vec![0.0; cnf.node];
    let mut fs_low = vec![0.0; cnf.node];
    let mut fs_high = vec![0.0; cnf.node];

    for i in 1..(cnf.node - 2) {
        fs_low[i] =
            0.5 * (cnf.a * us[i - 1] + cnf.a * us[i]) - 0.5 * f64::abs(cnf.a) * (us[i] - us[i - 1]);
    }

    // lax_wendroff
    for i in 2..(cnf.node - 1) {
        us_star[i] = 0.5 * (us[i] + us[i - 1] - 0.5 * cnf.dt * (us[i] - us[i - 1])) / cnf.dx;
        fs_high[i] = cnf.a * us_star[i];
    }

    // flux: TVD リミッターで流速を計算する
    for i in 1..(cnf.node - 2) {
        let reg_term: f64 = sign(us[i + 1] - us[i]) * 1.0e-5;
        let r_l = (us[i] - us[i - 1]) / (us[i + 1] - us[i] + reg_term);
        let r_r = (us[i + 2] - us[i + 1]) / (us[i + 1] - us[i] + reg_term);
        let phi = minmod1(r_l, r_r);
        fs[i + 1] = fs_low[i + 1] + phi * (fs_high[i + 1] - fs_low[i + 1]);
    }

    fs
}

fn minmod1(x: f64, y: f64) -> f64 {
    if x >= 0.0 && y >= 0.0 {
        f64::min(1.0, f64::min(x, y))
    } else {
        0.0
    }
}
