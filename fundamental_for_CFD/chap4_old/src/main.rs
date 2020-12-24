extern crate chap4;
extern crate chrono;
use chap4::Config;
use chap4::*;
use chrono::Local;
use std::env;
use std::fs;
use std::process;

fn main() {
    let args: Vec<String> = env::args().collect();
    let cnf = Config::new(&args).unwrap_or_else(|err| {
        println!("引数指定に問題があります: {}", err);
        process::exit(1);
    });

    let dir_name = Local::now()
        .format(&format!(
            "workspace/%Y%m%d-%H%M%S_chap4_{}_{}_{}_{}",
            &cnf.init_string, &cnf.reconstruction_string, &cnf.limiter_string, &cnf.riemann_string
        ))
        .to_string();

    // CSv 出力用ディレクトリがなければ作る
    fs::create_dir_all(&dir_name).unwrap_or_else(|why| {
        println!("! {:?}", why.kind());
    });

    // 初期化
    let mut n: i64 = 0;
    let mut t: f64 = 0.0;
    let (xs, mut us) = init(&cnf);
    let ues = exact(&cnf, t, &xs);
    let uls = vec![0.0; cnf.node];
    let urs = vec![0.0; cnf.node];
    let fs = vec![0.0; cnf.node];

    // 初期自国でのデータ記録
    write_csv(&dir_name, &n, &cnf.node, &xs, &us, &ues, &uls, &urs, &fs)
        .map_err(|err| println!("{:?}", err))
        .ok();

    while t <= cnf.tstop {
        n = n + 1;
        t = t + cnf.dt;

        let (uls, urs) = reconstruction(&cnf, &us);
        let fs = riemann(&cnf, &uls, &urs);
        us = update(&cnf, &us, &fs);
        let ues = exact(&cnf, t, &xs);

        write_csv(&dir_name, &n, &cnf.node, &xs, &us, &ues, &uls, &urs, &fs)
            .map_err(|err| println!("{:?}", err))
            .ok();
    }
    println!("1d_mp4.py を実行して動画ファイルを生成してください.");
}
