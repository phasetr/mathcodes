int itr = 0;
float scalar = 30;
void setup() {
  size(500, 500);
  translate(width / 2, height / 2);  //描画ウィンドウの中心に移動
  stroke(0, 0, 255);
  drawLine(10); //中心角の等分線を描く
  stroke(255, 0, 0);
  drawRealCurve(1.0 / 10);  //連続的なフェルマーらせんを描く
}
void draw() {
  translate(width / 2, height / 2);  //描画ウィンドウの中心に移動
  noStroke(); //輪郭線を消す
  drawFermatSpiral(1.0 / 10);
  itr++;
}
void drawFermatSpiral(float rot){
  float theta = 2 * PI * itr * rot; //回転角
  PVector v = PVector.fromAngle(theta);
  v.mult(scalar * sqrt(itr));
  fill(0);
  ellipse(v.x, v.y, 10, 10); //点を描画
}
