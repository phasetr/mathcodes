void makeLattice(){
  int m = ceil(num / (base[1].x - 1.0 / sqrt(3))); //列の数
  lattice = new PVector[num + 1][m + 1];
  for (int i = 0; i <= num; i++){
    for (int j = 0; j <= m; j++){
      PVector v = PVector.mult(base[0], i * scalar);
      v.add(PVector.mult(base[1], j * scalar));
      lattice[i][j] = new PVector(v.x, v.y % (height + scalar));
    }
  }
}
