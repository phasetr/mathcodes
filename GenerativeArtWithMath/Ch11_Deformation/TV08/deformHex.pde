void deformHex(){
  PVector[] v = new PVector[6];
  for (int i = 0; i < 6; i++){
    v[i] = PVector.fromAngle(2 * PI * i / 6);  //正六角形の頂点
    v[i].mult(scalar / sqrt(3));
    v[i] = parameterizeTV08(v, i);  //各頂点に対する変形
  }
  tile = createShape();
  tile.beginShape();
  for (int i = 0; i < 6; i++){
    tile.vertex(v[i].x, v[i].y);
  }
  tile.endShape(CLOSE);
}
