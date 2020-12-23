int num = 5; //描画するタイルの行の数
PVector[][] lattice;  //格子点ベクトル
PShape tile;  //タイル
PVector[] base = new PVector[2];  //格子を張るベクトル
float scalar;  //タイルの辺の長さ
void setup(){
  size(500, 500);
  colorMode(HSB, 1);
  scalar = height * 1.0 / num; //描画ウィンドウと行の数からタイルの大きさを決定
  makeSqVector();  //正方格子を張るベクトルの生成
  makeSqLattice();  //正方格子の格子点ベクトルを生成
  makeSqTriangle(); //正方形タイルを生成
  drawTiling();  //タイリングを描画
}
void draw(){}
void mouseClicked(){  //マウスクリック時の動作
  makeSqTriangle(); //正方形タイルを生成
  drawTiling();
}
