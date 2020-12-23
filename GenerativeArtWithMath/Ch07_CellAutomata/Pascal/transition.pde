int transition(int i){
  int nextC = state[i + 1] + state[i];  //パスカルの法則に従った計算
  return nextC;
}
