void initialize(int[][] mtx){
  for (int i = 0; i < mtx.length; i++){
    for (int j = 0; j < mtx[0].length; j++){
      mtx[i][j] = 0;  //行列のすべての要素を0にする
    }
  }
}
