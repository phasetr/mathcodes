PShape makeCurve(float[] rand){  //模様1(ベジエ曲線)
  PVector[] v = new PVector[2]; //二等辺三角形の2斜辺の中点
  for (int i = 0; i < 2; i++){
    v[i] = PVector.fromAngle(2 * PI * i / 3 + PI / 6);
    v[i].mult(scalar / 6);
  }
  PVector[] ctr = new PVector[4]; //ベジエ曲線の制御点
  for (int i = 0; i < 4; i++){
    ctr[i] = PVector.sub(v[(i+1)%2], v[i%2]);
    ctr[i].rotate(rand[i] * PI / 3);
    ctr[i].add(v[i%2]);
  }
  PShape pat = createShape();
  pat.beginShape(); //ベジエ曲線の生成
  pat.vertex(v[0].x, v[0].y);
  pat.bezierVertex(ctr[0].x, ctr[0].y,
    ctr[1].x, ctr[1].y,
    v[1].x, v[1].y);
  pat.bezierVertex(ctr[3].x, ctr[3].y,
    ctr[2].x, ctr[2].y,
    v[0].x, v[0].y);
  pat.endShape();
  return pat;
}
