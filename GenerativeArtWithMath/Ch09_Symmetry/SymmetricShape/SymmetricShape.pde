PShape crv; //ベジエ曲線
int gon = 10;
void setup(){
  size(500, 500, P2D);  //ベジエ曲線を描画する場合はP2Dレンダラを使った方が無難
  colorMode(HSB, 1);
  background(0, 0, 1);
  makeCurve();  //ベジエ曲線の描画
  drawShape();  //二面体群による変換と描画
}
