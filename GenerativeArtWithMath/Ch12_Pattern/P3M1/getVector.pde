PVector[] getVector(PVector[] v, float gap){
  PVector[] nextVec = new PVector[3];
  for(int i = 0; i < 3; i++){
    PVector dir = PVector.sub(v[(i+1)%3], v[i]);
    dir.mult(gap);
    nextVec[i] = PVector.add(v[i], dir);
  }
  return nextVec;
}
