PShape makeTriangle(float[] rand){
  PVector[] v = new PVector[2];
  for(int i = 0; i < 2; i++){
    v[i] = PVector.fromAngle(i * PI / 6);
    v[i].mult(scalar);
  }
  PVector[] ctr = new PVector[4];
  for (int i = 0; i < 4; i++){
    ctr[i] = PVector.mult(v[i / 2], rand[i]);
  }
  PShape tri = createShape();
  tri.beginShape();
  tri.vertex(0, 0);
  tri.vertex(ctr[0].x, ctr[0].y);
  tri.bezierVertex(ctr[1].x, ctr[1].y,
    ctr[2].x, ctr[2].y,
    ctr[3].x, ctr[3].y);
  tri.endShape(CLOSE);
  return tri;
}
