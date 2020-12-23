PVector[] parameterizeIH01(PVector[] v, int i, float[][] rand){
  PVector[] w = new PVector[2];
  for (int j = 0; j < 2; j++){
    w[j] = PVector.sub(v[(i + 1) % 6], v[i]); //ベジエ曲線の始点から終点までのベクトル
    w[j].mult(pow(-1, j));  //j=1ならば始点と終点を入れ替える
    if (i < 3){
      w[j].rotate(rand[i % 3][j % 2] * PI / 3);
    } else {
      w[j].rotate(rand[i % 3][(j + 1) % 2] * PI / 3);
    }
    w[j].add(v[(i + j) % 6]);
  }
  return w; //3次ベジエ曲線の4つの制御点のうち，中間の2点を返す
}
