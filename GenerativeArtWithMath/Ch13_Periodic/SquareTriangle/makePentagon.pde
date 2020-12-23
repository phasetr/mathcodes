PShape makePentagon(PVector[] v, color[] col, float gap){
  PVector[] w = new PVector[4]; //小さな正方形の頂点
  for(int i = 0; i < 4; i++){
    w[i] = PVector.sub(v[(i + 1) % 4], v[i]);
    w[i].mult(gap);
    w[i].add(v[i]);
  }
  PVector[] u = new PVector[4];  //五角形の頂点(点4,5,6,7)
  float theta = atan(gap);  //ずれの角度
  for(int i = 0; i < 4; i++){
    u[i] = PVector.sub(v[(i + 1) % 4], w[i]);
    u[i].mult(0.5 / pow(cos(theta), 2));
    u[i].add(w[i]);
  }
  PShape pent = createShape(GROUP);  //半分の五角形のグループ
  for (int i = 0; i < 4; i++){
    PShape halfPent = createShape();
    halfPent.setFill(col[i]);
    halfPent.beginShape();
    halfPent.vertex(v[i].x, v[i].y);
    halfPent.vertex(u[(i + 3) % 4].x, u[(i + 3) % 4].y);
    halfPent.vertex(0, 0);
    halfPent.vertex(u[i].x, u[i].y);
    halfPent.endShape();
    pent.addChild(halfPent);
  }
  return pent;
}
