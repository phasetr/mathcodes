PVector[][] lattice;
PShape tile;
PVector[] base = new PVector[2];  //格子を張るベクトル
int num = 2;
float scalar;
void setup(){
  size(500, 500);
  scalar = height * 1.0 / num;
  makeHexVector();
  makeLattice();
  makePatternP3M1();
  drawTiling();
}
void draw(){}
void mouseClicked(){  //マウスクリック時の動作
  makePatternP3M1();
  drawTiling();
}
