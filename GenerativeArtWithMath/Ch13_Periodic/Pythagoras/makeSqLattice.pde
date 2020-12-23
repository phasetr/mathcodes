void makeSqLattice(){
  lattice = new PVector[num + 1][num + 1];  
  for (int i = 0; i < num + 1; i++){
    for (int j = 0; j < num + 1; j++){
      PVector v = PVector.mult(base[0], i * scalar);  //正方形を描画する位置ベクトル
      v.add(PVector.mult(base[1], j * scalar));
      lattice[i][j] = new PVector(v.x, v.y);
    }
  }
}
