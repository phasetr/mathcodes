PShape makeRecurTriangle(float gap){
  PVector[] v = new PVector[3]; //正三角形の頂点
  v[2] = new PVector(0, 0);
  for (int i = 0; i < 2; i++){
    v[i] = PVector.fromAngle(i * PI / 3);
    v[i].mult(scalar / sqrt(3));
  }
  PShape tri = createShape();
  tri.beginShape(TRIANGLES);  //3点ずつの頂点から三角形を作る
  while (v[0].dist(v[1]) > 1){
    for(int i = 0; i < 3; i++){
      tri.vertex(v[i].x, v[i].y);
    }
    v = getVector(v, gap);  //gapの分だけずらした正三角形の頂点を取得
  }
  tri.endShape();
  return tri;
}
