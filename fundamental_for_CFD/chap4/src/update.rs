use crate::config::Config;
use crate::init::Init;

pub fn initialize(cnf: &Config) -> (Vec<f64>, Vec<f64>) {
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
        Init::Square => ues = exact_square(&cnf, t),
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

pub fn update(cnf: &Config, us: &Vec<f64>, fs: &Vec<f64>) -> Vec<f64> {
    let mut us_new = us.to_vec();

    for i in 2..(cnf.node - 2) {
        us_new[i] = us[i] - cnf.dt / cnf.dx * (fs[i + 1] - fs[i]);
    }

    bc(cnf, &us_new)
}

pub fn bc(cnf: &Config, us: &Vec<f64>) -> Vec<f64> {
    let mut us_new = us.to_vec();
    us_new[0] = us_new[cnf.i_max - 3];
    us_new[1] = us_new[cnf.i_max - 2];
    us_new[cnf.i_max - 1] = us_new[2];
    us_new[cnf.i_max] = us_new[3];
    us_new
}
