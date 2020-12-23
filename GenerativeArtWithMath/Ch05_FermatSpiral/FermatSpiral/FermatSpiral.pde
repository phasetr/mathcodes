int itr = 0;  //描画の繰り返し回数
float scalar = 5; //拡大倍率
void setup() {
  size(500, 500);
  background(255);  //背景を白くする
}
void draw() {
  translate(width / 2, height / 2);  //描画ウィンドウの中心に移動
  fill(0);  //点を黒く塗る
  drawFermatSpiral(17.0 / 55);  //引数を回転角とするフェルマーらせんの描画
  itr++;
}
void drawFermatSpiral(float rot){
  float theta = 2 * PI * itr * rot; //回転角
  PVector v = PVector.fromAngle(theta);
  v.mult(scalar * sqrt(itr));
  ellipse(v.x, v.y, scalar, scalar); //点を描画
}
