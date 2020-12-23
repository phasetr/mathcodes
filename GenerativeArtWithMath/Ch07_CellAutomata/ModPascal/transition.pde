int transition(int i){
  int nextC = (state[i + 1] + state[i]) % mod;  // 遷移規則
  return nextC;
}
