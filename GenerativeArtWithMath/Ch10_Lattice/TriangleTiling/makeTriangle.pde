PShape makeTriangle(){
  PShape tri = createShape();
  tri.beginShape();
  for (int i = 0; i < 3; i++){
    PVector v = PVector.fromAngle(2 * PI * i / 3 + PI / 2); //正三角形の頂点
    v.mult(scalar / pow(sqrt(3), 2));
    tri.vertex(v.x, v.y);
  }
  tri.endShape(CLOSE);
  return tri;
}
