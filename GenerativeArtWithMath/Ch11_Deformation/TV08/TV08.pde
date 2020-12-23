import controlP5.*;
ControlP5 cp5;
PVector[][] lattice;  //格子
PShape tile;  //タイル
PVector[] base = new PVector[2];  //格子を張るベクトル
int row = 10; //タイルの行の数
int col;  //タイルの列の数
float scalar; //拡大倍率
color[][] tileColor;  //タイルの色
float hor, ver; //水平方向，垂直方向へのずれの変数
void setup(){
  size(500, 500);
  colorMode(HSB, 1);
  scalar = height * 1.0 / row;
  controller(); //controlP5コントローラの設定
  makeHexVector();  //六角格子を張るベクトルの生成
  col = ceil(row / (base[1].x - 1.0 / sqrt(3)));  //タイルの列の数を計算
  setTileColor(); //タイルにランダムに色をセット
}
void draw(){
  background(1, 0, 1);
  deformLattice();  //格子の生成
  deformHex(); //タイルの生成
  drawTiling();  //タイリングを描画
}
