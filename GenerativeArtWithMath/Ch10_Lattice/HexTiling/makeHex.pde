void makeHex(){
  tile = createShape();
  tile.beginShape();
  for (int i = 0; i < 6; i++){
    PVector v = PVector.fromAngle(2 * PI * i / 6);  //正六角形の頂点
    v.mult(scalar / sqrt(3));
    tile.vertex(v.x, v.y);
  }
  tile.endShape(CLOSE);
}
