void makeCurve(){
  PVector[] v = new PVector[2];
  for(int i = 0; i < 2; i++){
    v[i] = PVector.fromAngle(i * PI / gon); //基本領域のベクトル
    v[i].mult(width / 2);
  }
  PVector[] ctr = new PVector[4];
  for (int i = 0; i < 4; i++){
    ctr[i] = PVector.mult(v[i/2], random(1)); //制御点をランダムに取る
  }
  crv = createShape();
  crv.setFill(color(random(1), 1, 1));
  crv.beginShape();
  crv.vertex(0, 0);
  crv.vertex(ctr[0].x, ctr[0].y);
  crv.bezierVertex(ctr[1].x, ctr[1].y, ctr[2].x, ctr[2].y, ctr[3].x, ctr[3].y);
  crv.endShape(CLOSE);
}
