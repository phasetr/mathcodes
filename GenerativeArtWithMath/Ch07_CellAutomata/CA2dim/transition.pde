int transition(int i, int j){
  int nextC;
    nextC = state[(i - 1 + num) % num][j] //上のセル
      + state[i][(j - 1 + num) % num]  //左のセル
      + state[i][j] //中央のセル
      + state[i][(j + 1) % num] //右のセル
      + state[(i + 1) % num][j];  //下のセル
  nextC = nextC % mod;
  return nextC;
}
