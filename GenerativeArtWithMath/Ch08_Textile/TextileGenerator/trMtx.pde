int[][] trMtx(int[][] mtx){ //行列の転置
  int[][] newMtx = new int[mtx[0].length][mtx.length];
  for (int i = 0; i < mtx.length; i++){
    for (int j = 0; j < mtx[0].length; j++){
      newMtx[j][i] = mtx[i][j]; //要素を入れ替える
    }
  }
  return newMtx;
}
