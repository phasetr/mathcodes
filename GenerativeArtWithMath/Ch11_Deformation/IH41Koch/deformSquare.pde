void deformSquare(){
  PVector[] v = new PVector[4];
  for (int i = 0; i < 4; i++){
    v[i] = PVector.fromAngle(2 * PI * (i + 0.5) / 4);
    v[i].mult(scalar / sqrt(2));
  }
  tile = createShape();
  tile.beginShape();
  for (int i =0; i < 4; i++){
    if (i < 2){
      makeKoch(v[i], v[(i+1)%4], 0, true);
    } else {
      makeKoch(v[i], v[(i+1)%4], 0, false);
    }
  }
  tile.endShape(CLOSE);
}
