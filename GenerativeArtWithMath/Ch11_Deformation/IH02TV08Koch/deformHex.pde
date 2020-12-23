void deformHex(){
  PVector[] v = new PVector[6];
  for (int i = 0; i < 6; i++){
    v[i] = PVector.fromAngle(2 * PI * i / 6);
    v[i].mult(scalar / sqrt(3));
    v[i] = parameterizeTV08(v, i);
  }
  tile = createShape();
  tile.beginShape();
  for (int i =0; i < 6; i++){
    if (i < 3){
      makeKoch(v[i], v[(i+1)%6], 0, true);
    } else {
      makeKoch(v[i], v[(i+1)%6], 0, false);
    }
  }
  tile.endShape(CLOSE);
}
