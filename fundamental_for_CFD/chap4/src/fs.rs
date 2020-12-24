use std::error;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufWriter;

pub fn write_csv(
    dir_name: &str,
    n: &i64,
    node: &usize,
    xs: &Vec<f64>,
    us: &Vec<f64>,
    ues: &Vec<f64>,
    uls: &Vec<f64>,
    urs: &Vec<f64>,
    fs: &Vec<f64>,
) -> Result<(), Box<dyn error::Error>> {
    let file_name: String = format!("{}/{:08}.csv", &dir_name, &n);
    let file = File::create(file_name).unwrap();
    let mut w = BufWriter::new(file);
    write!(w, "xs,us,ues,uls,urs,fs\n").unwrap();
    for i in 0..*node {
        let s = format!(
            "{},{},{},{},{},{}\n",
            xs[i], us[i], ues[i], urs[i], uls[i], fs[i]
        );
        // unwrap を呼んで書き込みエラーを検知
        write!(w, "{}", s).unwrap();
    }
    // flush を呼ぶことで書き込みエラーを全て拾える
    w.flush().unwrap();
    Ok(())
}
