PShape makeTriangle(float[] rand){
  PVector[] v = new PVector[2]; //正三角形を二等分する線分の端点
  v[0] = new PVector(0, 0); //頂点
  v[1] = PVector.fromAngle(PI / 6); //辺の中点
  v[1].mult(scalar / 2);
  PVector[] ctr = new PVector[2];
  for (int i = 0; i < 2; i++){
    ctr[i] = PVector.sub(v[(i + 1) % 2], v[i]);
    ctr[i].rotate(rand[i] * PI / 3);
    ctr[i].add(v[i]);
  }
  PShape tri = createShape();
  tri.beginShape(); //ベジエ曲線の生成
  tri.noFill();
  tri.strokeWeight(3);
  tri.vertex(v[0].x, v[0].y);
  tri.bezierVertex(ctr[0].x, ctr[0].y,
    ctr[1].x, ctr[1].y,
    v[1].x, v[1].y);
  tri.endShape();
  return tri;
}
