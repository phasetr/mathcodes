int transition(int a, int b, int c){
  int d;
  if (random(1) < 0.999){
    d = a + b + c;  //99.9%の確率でこのルールを選択
  } else {
    d = a + c;  //0.1%の確率でこのルールを選択
  }
  d = d % mod;
  return d;
}
