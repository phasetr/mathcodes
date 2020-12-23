PShape crv;
PVector v1 = new PVector(0, 250);  //始点
PVector v2 = new PVector(500, 250);  //終点
int upperLimit = 0;  //操作の繰り返し回数
void setup(){
  size(500, 500);
  colorMode(HSB, 1);
  background(1, 0, 1);
  drawCurve();  //コッホ曲線の描画
}
void drawCurve(){
  crv = createShape();
  crv.beginShape();
  makeKoch(v1, v2, 0);  //コッホ曲線の頂点を与える
  crv.endShape();
  shape(crv);
}
