use std::process::Command;
fn main() {
    let output = Command::new("ls")
        .args(&["-l", "-a"])
        .output()
        .expect("failed to start `ls`");
    println!("{}", String::from_utf8_lossy(&output.stdout));

    let options = [
        "-r",
        "10",
        "-i",
        "workspace/20200328-084442_4-8_burgers_1st_order_Lax-Friedrichs_sin/img.%08d.png",
        "output.mp4",
    ];

    let output2 = Command::new("ffmpeg")
        .args(&options)
        .output()
        .expect("failed to start `ffmpeg`");
    println!("{}", String::from_utf8_lossy(&output2.stdout));
}
