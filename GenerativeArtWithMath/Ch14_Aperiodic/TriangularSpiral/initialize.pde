void initialize(float scalar){
  PVector v0 = PVector.fromAngle(3 * PI / 2); //最も角度の狭い鋭角
  v0.mult(scalar);
  PVector v1 = PVector.fromAngle(7 * PI / 10);
  v1.mult(scalar);
  PVector v2 = PVector.fromAngle(3 * PI / 10);
  v2.mult(scalar);
  t = new Tri(v0, v1, v2);  //細い三角形の生成
}
