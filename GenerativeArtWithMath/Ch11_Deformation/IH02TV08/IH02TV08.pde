import controlP5.*;
ControlP5 cp5;
PVector[][] lattice;
PShape tile;
PVector[] base = new PVector[2];  //格子を張るベクトル
int row = 10;
int col;
float scalar;
color[][] tileColor;
float[][] rand;
float hor, ver; //水平方向，垂直方向へのずれの変数
void setup(){
  size(500, 500, P2D);
  colorMode(HSB, 1);
  scalar = height * 1.0 / row;
  controller();
  makeHexVector();
  col = ceil(row / (base[1].x - 1.0 / sqrt(3)));
  randomize();
}
void draw(){
  background(1, 0, 1);
  deformLattice();  //格子の生成
  deformHex(); //タイルの生成
  drawTiling();  //タイリングを描画
}
void randomize(){
  rand = new float[3][2];
  for (int i = 0; i < 3; i++){  //ベジエ曲線の制御点生成のための乱数
    rand[i][0] = random(-1, 1);
    rand[i][1] = random(-1, 1);
  }
  setTileColor();
}
