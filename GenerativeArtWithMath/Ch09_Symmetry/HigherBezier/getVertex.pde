PVector[] getVertex(PVector[] v, float t){
  PVector[] newVtx = new PVector[v.length - 1];
  for (int i = 0; i < v.length - 1; i++){
    newVtx[i] = PVector.sub(v[i + 1],v[i]);
    newVtx[i].mult(t);
    newVtx[i].add(v[i]);
  }
  return newVtx;
}
