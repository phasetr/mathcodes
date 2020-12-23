int transition(int a, int b, int c){
  int d = a + b + c;  //遷移ルールに従って計算
  d = d % mod;
  return d;
}
