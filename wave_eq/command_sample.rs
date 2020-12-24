use std::process::Command;

const PI: f64 = std::f64::consts::PI;

fn main() {
    let a = Command::new("ls")
        .arg("-la")
        .output()
        .expect("failed to start `ffmpeg`");
    println!("{}", String::from_utf8_lossy(&a.stdout));

    let b = Command::new("gnuplot")
        .arg("-e")
        .arg(r#"set terminal png"#)
        .arg("-e")
        .arg(r#"set datafile separator ",""#)
        .arg("-e")
        .arg(r#"set output "command_sample.tmp.png""#)
        .arg("-e")
        .arg(r#"set ticslevel 0; set dgrid3d 100,100;"#)
        .arg("-e")
        .arg(r#"splot "gnuplot_oneliner.csv" u 1:2:3 with lines;"#)
        .output()
        .expect("failed to start `ffmpeg`");
    println!("{}", String::from_utf8_lossy(&b.stdout));
}
