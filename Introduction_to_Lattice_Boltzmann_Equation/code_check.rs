// cargo-deps: serde, serde_derive, serde_yaml, ndarray
extern crate ndarray;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate serde_yaml;
use ndarray::array;
use ndarray::prelude::*;
use ndarray::Array;
use ndarray::Array1;
use ndarray::Array4;
//use serde::{Deserialize, Serialize};
use std::fs::File;
use std::io::prelude::*;
use std::io::BufWriter;

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct Point {
    x: f64,
    y: f64,
}

fn main() -> Result<(), serde_yaml::Error> {
    let a1: Array1<f64> = array![1.0, 2.0, 3.0];
    println!("{:?}", 3.0 * a1 + 2.0);
    println!("ndarray のベクトル計算");
    let arr1: Array1<f64> = array![1.0, 2.0, 3.0, 4.0];
    let arr2: Array1<f64> = array![5.0, 6.0, 7.0, 8.0];
    let mut arr_sum1: Array1<f64> = Array::zeros(4);
    azip!((
        c in arr_sum1.slice_mut(s![0..2]),
        a in arr1.slice(s![1..3]),
        b in arr2.slice(s![2..4])) {
        *c = a + b;
    });
    println!("{}", &arr_sum1);
    println!("=======================");

    // べき乗をどう書くか
    let a: f64 = 3.0;
    let b = a.powf(2.0);
    println!("{}", b);
    let c: f64 = 2.0;
    let d: f64 = 2.0;
    let e: f64 = 2.0;
    println! {"{} {} {}", &c, &d, &e};

    let arr: Array4<f64> = Array::zeros((3, 2, 2, 2));
    println!("{}", &arr);

    // yaml 書き込み
    let point = Point { x: 1.0, y: 2.0 };

    let s = serde_yaml::to_string(&point)?;
    assert_eq!(s, "---\nx: 1.0\ny: 2.0");
    println!("{}", s);

    let deserialized_point: Point = serde_yaml::from_str(&s)?;
    assert_eq!(point, deserialized_point);

    let file_name: String = format!("1.tmp.yaml");
    let file = File::create(file_name).unwrap();
    let mut w = BufWriter::new(file);
    write!(w, "{}", &s).unwrap();
    w.flush().unwrap();

    Ok(())
}
