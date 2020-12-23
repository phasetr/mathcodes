int itr = 0;  //描画の繰り返し回数
float scalar = 5; //拡大倍率
void setup() {
  size(500, 500);
  background(255);
}
void draw() {
  translate(width / 2, height / 2);  //描画ウィンドウの中心に移動

  noStroke();
  fill(255, 0, 0, 127);  //点を赤く塗る
  drawFermatSpiral(1.0 / 3);
  // drawFermatSpiral(4.0 / 17);
  fill(0, 0, 255, 127);  //点を青く塗る
  drawFermatSpiral(1.0 / 61);
  // drawFermatSpiral(17.0 / 72);
  fill(0, 255, 0, 127);  //点を緑に塗る
  drawFermatSpiral(20.0 / 61);
  // drawFermatSpiral(72.0 / 305);
  itr++;
}
void drawFermatSpiral(float rot){
  float theta = 2 * PI * itr * rot; //回転角
  PVector v = PVector.fromAngle(theta);
  v.mult(scalar * sqrt(itr));
  ellipse(v.x, v.y, scalar, scalar); //点を描画
}
