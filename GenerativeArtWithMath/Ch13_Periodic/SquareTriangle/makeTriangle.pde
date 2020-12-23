PShape makeTriangle(PVector[] v, color col, float gap){
  PVector[] w = new PVector[4]; //小さな正方形の頂点(点4,5,6,7)
  for(int i = 0; i < 4; i++){
    w[i] = PVector.sub(v[(i + 1) % 4], v[i]);
    w[i].mult(gap); //正方形の頂点をずらす
    w[i].add(v[i]);
  }
  PShape tri = createShape(GROUP);  //半分の三角形のグループ
  for(int i = 0; i < 4; i++){
    PShape halfTri = createShape(); //半分の三角形を作る
    halfTri.setFill(col);
    halfTri.beginShape();
    halfTri.vertex(v[i].x, v[i].y);
    halfTri.vertex(w[i].x, w[i].y);
    halfTri.vertex(w[(i + 3) % 4].x, w[(i + 3) % 4].y);
    halfTri.endShape();
    tri.addChild(halfTri);
  }
  return tri;
}
