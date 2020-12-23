PVector[] parameterizeIH41(PVector[] v, int i, float[][] rand){
  PVector[] w = new PVector[2];
  for (int j = 0; j < 2; j++){
    w[j] = PVector.sub(v[(i + 1) % 4], v[i]);
    w[j].mult(pow(-1, j));  //j=1ならば始点と終点を入れ替える
    if(i < 2){
      w[j].rotate(rand[i % 2][j % 2] * PI / 4);
    } else {
      w[j].rotate(rand[i % 2][(j + 1) % 2] * PI / 4);
    }
    w[j].add(v[(i + j) % 4]);
  }
  return w;
}
