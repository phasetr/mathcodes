void deformLattice(){
  lattice = new PVector[row + 1][col + 1];
  for (int i = 0; i < row + 1; i++){
    for (int j = 0; j < col + 1; j++){
      PVector v = PVector.mult(base[0], i * scalar);
      v.add(PVector.mult(base[1], j * scalar));
      v.add(hor * scalar * j / sqrt(3), 0);  //水平方向へ格子をずらす
      lattice[i][j] = new PVector(v.x, v.y % (height + scalar));
    }
  }
}
