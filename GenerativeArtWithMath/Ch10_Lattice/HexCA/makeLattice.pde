void makeLattice(){
  lattice = new PVector[num][num];
  for (int i = 0; i < num; i++){
    for (int j = 0; j < num; j++){
      PVector v = PVector.mult(base[0], i * scalar);
      v.add(PVector.mult(base[1], j * scalar));
      lattice[i][j] = new PVector(v.x, v.y % height);
    }
  }
}
