int[][] multMtx(int[][] mtx1, int[][] mtx2){  //mtx1とmtx2をかけて返す
  int[][] newMtx = new int[mtx1.length][mtx2[0].length];
  for (int i = 0; i < mtx1.length; i++){
    for (int j = 0; j < mtx2[0].length; j++){
      int sum = 0;  //(i,j)要素の初期値
      for (int k = 0; k < mtx2.length; k++){
        sum += mtx1[i][k] * mtx2[k][j]; //要素をかけて足す
      }
      newMtx[i][j] = sum;
    }
  }
  return newMtx;
}
