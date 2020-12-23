PVector[] getMidPoint(PVector[] v, float t){
  PVector[] pt = new PVector[v.length - 1];
  for (int i = 0; i < v.length - 1; i++){
    pt[i] = PVector.sub(v[i + 1],v[i]);
    pt[i].mult(t);
    pt[i].add(v[i]);
  }
  return pt;
}
