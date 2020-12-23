PVector[][] lattice;
PShape tile;
PVector[] base = new PVector[2];  //格子を張るベクトル
int num = 10;
float scalar;
void setup(){
  size(500, 500);
  colorMode(HSB, 1);
  scalar = height * 1.0 / num;
  makeHexVector();  //六角格子を張るベクトルの生成
  makeLattice();  //格子点ベクトルを生成
  makeHexTriangle();  //6つの正三角形に分割された正六角形タイルを生成
  drawTiling();  //タイリングを描画
}
void draw(){}
void mouseClicked(){  //マウスクリック時の動作
  drawTiling();
}
