import controlP5.*;
ControlP5 cp5;
int num = 10; //描画するタイルの行の数
PVector[][] lattice;  //格子点ベクトル
PShape tile;  //タイル
PVector[] base = new PVector[2];  //格子を張るベクトル
float scalar;  //タイルの辺の長さ
float gap;  //正方形をずらす角度を決めるパラメータ
color[] col = new color[2]; //正方形タイルの色
void setup(){
  size(500, 500);
  colorMode(HSB, 1);
  scalar = height * 1.0 / num; //描画ウィンドウと行の数からタイルの大きさを決定
  for (int i = 0; i < 2; i++){  //正方形タイルの色の指定
    col[i] = color(random(1), 0.4, 1);
  }
  controller();
  makeSqVector();  //正方格子を張るベクトルの生成
  makeSqLattice();  //正方格子の格子点ベクトルを生成
}
void draw(){
  makePythagoras();
  drawTiling();
}
