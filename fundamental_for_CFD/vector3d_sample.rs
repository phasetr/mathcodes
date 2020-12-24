// cargo-deps: vector3d
extern crate vector3d;
use vector3d::Vector3d;

fn main() {
    let v1: Vector3d<f64> = Vector3d {
        x: 0.0,
        y: 0.0,
        z: 0.0,
    };
    println!("{:?}", v1);

    let v2: Vector3d<f64> = Vector3d {
        x: 1.0,
        y: 2.0,
        z: 3.0,
    };
    let v3 = v1 + v2;
    println!("{:?}", v3);

    let a: f64 = 4.0;
    let v4 = v3 * a;
    println!("{:?}", v4);
}
