void makeHex(){
  tile = createShape();
  tile.beginShape();
  tile.noStroke();
  for (int i = 0; i < 6; i++){
    PVector v = PVector.fromAngle(2 * PI * i / 6);
    v.mult(scalar / sqrt(3));
    tile.vertex(v.x, v.y);
  }
  tile.endShape(CLOSE);
}
