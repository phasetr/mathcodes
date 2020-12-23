PVector[][] lattice;  //格子のための変数
PShape tile;  //正方形タイルのための変数
int num = 10; //行の数
PVector[] base = new PVector[2];  //格子を張るベクトル
float scalar;  //正方形タイルの辺の長さ
void setup(){
  size(500, 500, P2D);
  colorMode(HSB, 1);
  scalar = height * 1.0 / num;
  makeSqVector();
  makeSqLattice();  //格子点ベクトルを生成
  deformSquare(); //正方形タイルを生成
  drawTiling();  //タイリングを描画
}
