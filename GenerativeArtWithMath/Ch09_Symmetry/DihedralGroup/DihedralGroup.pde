PShape img; //画像用の変数
PShape polygon; //正多角形用の変数
int gon = 6;  //正多角形の角数
float scalar;  //サイズ
int j = 1;  //鏡映変換のパラメータ
int k = 0;  //回転変換のパラメータ
void setup(){
  size(300, 300);
  scalar = height * 0.4;
  img = loadShape("F.svg"); //画像ファイル読み込み
  // img = loadShape("yosegi1.svg");  //寄木1の画像
  // img = loadShape("yosegi2.svg");  //寄木2の画像
  polygon = createShape();
  polygon.beginShape(); //頂点をつないでかたちを作る
  polygon.noFill();
  for (int i = 0; i < gon; i++){
    PVector v = PVector.fromAngle(2 * PI * i / gon);
    v.mult(scalar);
    polygon.vertex(v.x, v.y);  //正多角形の頂点
  }
  polygon.endShape(CLOSE);  //すべての頂点をつなぐ
  drawShape();  //描画
}
