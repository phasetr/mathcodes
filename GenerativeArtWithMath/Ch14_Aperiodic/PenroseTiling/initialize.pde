void initialize(float scalar){
  col = new color[2];
  col[0] = color(random(1), 1, 1); //細い三角形の色
  col[1] = color(random(1), 1, 1);  //太い三角形の色
  PVector v0 = PVector.fromAngle(3 * PI / 2);
  v0.mult(scalar);
  PVector v1 = PVector.fromAngle(7 * PI / 10);
  v1.mult(scalar);
  PVector v2 = PVector.fromAngle(3 * PI / 10);
  v2.mult(scalar);
  listT.add(new Tri(v0, v1, v2));
}
