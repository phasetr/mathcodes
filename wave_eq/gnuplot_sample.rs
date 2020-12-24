// cargo-deps: chrono, gnuplot, ndarray
// https://github.com/SiegeLord/RustGnuplot/tree/master/examples
extern crate gnuplot;
use gnuplot::*;
use gnuplot::{MultiplotFillDirection::*, MultiplotFillOrder::*};
use std::error;

fn box_and_whisker() -> Result<(), Box<dyn error::Error>> {
    let mut fg = Figure::new();

    fg.axes2d()
        .set_title("Box and whisker", &[])
        .box_and_whisker(
            [0.0f32, 1.0, 2.0].iter(),
            [-1.0f32, 0.0, 1.0].iter(),
            [-2.0f32, -1.0, 0.0].iter(),
            [2.0f32, 3.0, 4.0].iter(),
            [1.0f32, 2.0, 3.0].iter(),
            &[],
        )
        .box_and_whisker_set_width(
            [-0.6f32, 1.5, 2.5].iter(),
            [-1.0f32, 0.0, 1.0].iter(),
            [-2.0f32, -1.0, 0.0].iter(),
            [2.0f32, 3.0, 4.0].iter(),
            [1.0f32, 2.0, 3.0].iter(),
            [0.5f32, 0.25, 0.125].iter(),
            &[
                WhiskerBars(0.5),
                Color("blue"),
                LineWidth(2.0),
                LineStyle(SmallDot),
                FillAlpha(0.5),
            ],
        )
        .set_x_range(Fix(-1.0), Fix(3.0))
        .set_y_range(Fix(-3.0), Fix(5.0));
    fg.save_to_png("gnuplot_sample_box_and_whisker.tmp.png", 800, 800)
        .map_err(|err| println!("{:?}", err))
        .ok();

    Ok(())
}

fn color_cycling() -> Result<(), Box<dyn error::Error>> {
    let x = 0..10;

    let mut fg = Figure::new();

    let ax = fg.axes2d();
    ax.set_title("Color cycling", &[]);
    ax.set_legend(Graph(0.2), Graph(0.9), &[], &[]);
    for i in 0..10 {
        ax.lines_points(
            x.clone(),
            x.clone().map(|v| v * 2 + i),
            &[Caption(&format!("{}", i))],
        );
    }
    fg.save_to_png("gnuplot_sample_color_cycling.tmp.png", 800, 800)
        .map_err(|err| println!("{:?}", err))
        .ok();

    Ok(())
}

fn dash_types() -> Result<(), Box<dyn error::Error>> {
    let x = 0..10;

    let mut fg = Figure::new();

    let ax = fg.axes2d();
    ax.set_title("Dash type", &[]);
    ax.set_legend(Graph(0.3), Graph(0.9), &[], &[]);
    for (i, &dt) in [Solid, SmallDot, Dot, Dash, DotDash, DotDotDash]
        .iter()
        .enumerate()
    {
        ax.lines(
            x.clone(),
            x.clone().map(|v| v * 2 + 2 * i),
            &[
                LineWidth(2.),
                Color("black"),
                LineStyle(dt),
                Caption(&format!("{:?}", dt)),
            ],
        );
    }
    fg.save_to_png("gnuplot_sample_dash_types.tmp.png", 800, 800)
        .map_err(|err| println!("{:?}", err))
        .ok();

    Ok(())
}

fn lines_3d() -> Result<(), Box<dyn error::Error>> {
    let z = (0..100).map(|z| z as f32 / 10.0);
    let x = z.clone().map(|z| z.cos());
    let y = z.clone().map(|z| z.sin());

    let mut fg = Figure::new();

    fg.axes3d().set_title("3D lines", &[]).lines(
        x,
        y,
        z,
        &[PointSymbol('o'), Color("#ffaa77"), PointSize(2.0)],
    );
    fg.save_to_png("gnuplot_sample_lines_3d.tmp.png", 800, 800)
        .map_err(|err| println!("{:?}", err))
        .ok();

    Ok(())
}

fn lines_points_3d() -> Result<(), Box<dyn error::Error>> {
    let z = (0..100).map(|z| z as f32 / 10.0);
    let x = z.clone().map(|z| z.cos());
    let y = z.clone().map(|z| z.sin());

    let mut fg = Figure::new();

    fg.axes3d()
        .set_title(r"3D lines + points", &[])
        .lines_points(
            x,
            y,
            z,
            &[PointSymbol('o'), Color("#ffaa77"), PointSize(2.0)],
        );
    fg.save_to_png("gnuplot_sample_lines_points_3d.tmp.png", 800, 800)
        .map_err(|err| println!("{:?}", err))
        .ok();

    Ok(())
}

fn multi_options() -> Result<(), Box<dyn error::Error>> {
    let mut fg = Figure::new();
    fg.set_multiplot_layout(2, 2)
        .set_title("Multiple parabolas")
        .set_scale(0.8, 0.8)
        .set_offset(0.0, 0.0)
        .set_multiplot_fill_order(RowsFirst, Upwards);

    fg.axes2d()
        .lines(
            &[-3., -2., -1., 0., 1., 2., 3.],
            &[9., 4., 1., 0., 1., 4., 9.],
            &[Caption("Parabola 1")],
        )
        .set_x_label("X label", &[])
        .set_title("Parabola 1", &[])
        .label("Test 1", Axis(-3.), Axis(-3.), &[])
        .label("Test 2", Axis(3.), Axis(3.), &[])
        .arrow(Axis(-3.), Axis(-3.), Axis(3.), Axis(3.), &[]);

    fg.axes2d().lines(
        &[-3., -2., -1., 0., 1., 2., 3.],
        &[10., 5., 2., 0., 2., 5., 10.],
        &[Caption("Parabola 2")],
    );

    fg.axes2d().lines(
        &[-3., -2., -1., 0., 1., 2., 3.],
        &[11., 6., 3., 0., 3., 6., 11.],
        &[Caption("Parabola 3")],
    );

    fg.save_to_png("gnuplot_sample_multi_options.tmp.png", 800, 800)
        .map_err(|err| println!("{:?}", err))
        .ok();

    Ok(())
}

fn patterns() -> Result<(), Box<dyn error::Error>> {
    let mut fg = Figure::new();

    let ax = fg.axes2d();
    ax.set_title("Patterns", &[]);
    ax.set_legend(Graph(1.), Graph(0.95), &[MaxRows(3)], &[]);
    ax.set_y_range(Auto, Fix(8.));
    for i in 0..=8 {
        ax.boxes_set_width(&[i], &[5], &[0.5], &[FillPattern(Auto)]);
    }

    for (i, &pattern) in [
        Pattern0,
        BigCrosses,
        SmallCrosses,
        Pattern3,
        BigBackSlashes,
        BigForwardSlashes,
        SmallForwardSlashes,
        SmallBackSlashes,
        Pattern8,
    ]
    .iter()
    .enumerate()
    {
        ax.boxes_set_width(
            &[i],
            &[-5],
            &[0.5],
            &[
                FillPattern(Fix(pattern)),
                Caption(&format!("{:?}", pattern)),
            ],
        );
    }

    fg.save_to_png("gnuplot_sample_patterns.tmp.png", 800, 800)
        .map_err(|err| println!("{:?}", err))
        .ok();

    Ok(())
}

fn points_3d() -> Result<(), Box<dyn error::Error>> {
    let z = (0..100).map(|z| z as f32 / 10.0);
    let x = z.clone().map(|z| z.cos());
    let y = z.clone().map(|z| z.sin());

    let mut fg = Figure::new();

    fg.axes3d().set_title("3D points", &[]).points(
        x,
        y,
        z,
        &[PointSymbol('o'), Color("#ffaa77"), PointSize(2.0)],
    );

    fg.save_to_png("gnuplot_sample_points3d.tmp.png", 800, 800)
        .map_err(|err| println!("{:?}", err))
        .ok();

    Ok(())
}

fn main() -> Result<(), Box<dyn error::Error>> {
    box_and_whisker();
    color_cycling();
    dash_types();
    lines_3d();
    lines_points_3d();
    multi_options();
    patterns();
    points_3d();

    Ok(())
}
