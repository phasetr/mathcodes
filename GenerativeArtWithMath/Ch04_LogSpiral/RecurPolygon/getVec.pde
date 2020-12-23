PVector[] getVector(PVector[] v){
  PVector[] nextVec = new PVector[gon];
  for(int i = 0; i < gon; i++){
    PVector dir = PVector.sub(v[(i + 1) % gon], v[i]);
    dir.mult(gap);
    nextVec[i] = PVector.add(v[i], dir);
  }
  return nextVec;
}
