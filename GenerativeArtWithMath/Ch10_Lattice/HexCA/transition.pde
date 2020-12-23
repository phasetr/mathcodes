int transition(int i, int j){
  int d;
  d = state[i][j] //中央のセル
    + state[(i - 1 + num) % num][j] //上のセル
    + state[(i - 1 + num) % num][(j + 1) % num]  //右上のセル
    + state[i][(j + 1) % num] //右下のセル
    + state[(i + 1) % num][j] //下のセル
    + state[(i + 1) % num][(j - 1 + num) % num] //左下のセル
    + state[i][(j - 1 + num) % num]; //左上のセル
  d = d % mod;
  return d;
}
