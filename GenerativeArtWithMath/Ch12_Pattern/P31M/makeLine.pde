PShape makeLine(float[] rand){  //模様2(三角形)
  PVector[] v = new PVector[2]; //二等辺三角形の底辺の2点をランダムに取る
  for (int i = 0; i < 2; i++){
    v[i] = PVector.fromAngle(PI / 6);
    v[i].mult(scalar / 3);
    PVector dir = new PVector(-scalar / sqrt(3), 0);
    dir.mult(abs(rand[i]));
    v[i].add(dir);
  }
  PShape pat = createShape();
  pat.beginShape(); //三角形の生成
  pat.vertex(0, 0);
  pat.vertex(v[0].x, v[0].y);
  pat.vertex(v[1].x, v[1].y);
  pat.endShape(CLOSE);
  return pat;
}
