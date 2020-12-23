void generateFibo(int ind){
  fibo = new int[]{0, 1};
  for(int i = 1; i < ind; i++){
    fibo = append(fibo, fibo[i-1] + fibo[i]);
  }
  fibo = reverse(fibo); //配列の番号付けを逆にする
}
