PVector[][] lattice;
PShape tile; //タイル
PVector[] base = new PVector[2];  //格子を張るベクトル
int num = 10;
float scalar;
void setup(){
  size(500, 500);
  colorMode(HSB, 1);
  scalar = height * 1.0 / num;
  makeHexVector();
  makeLattice();
  makeHexRhomb();  //正六角形タイルを生成
  drawTiling();
}
void draw(){}
void mouseClicked(){  //マウスクリック時の動作
  makeHexRhomb();
  drawTiling();
}
