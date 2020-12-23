PVector[] parameterizeIH01(PVector[] v, int i, float[][] rand){
  PVector[] w = new PVector[2];
  for (int j = 0; j < 2; j++){
    w[j] = PVector.sub(v[(i + 1) % 6], v[i]);
    w[j].mult(pow(-1, j));  //j=1ならば始点と終点を入れ替える
    if (i < 3){
      w[j].rotate(rand[i % 3][j % 2] * PI / 3);
    } else {
      w[j].rotate(rand[i % 3][(j + 1) % 2] * PI / 3);
    }
    w[j].add(v[(i + j) % 6]);
  }
  return w;
}
