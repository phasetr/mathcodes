void mouseClicked() {
  int nextFibo = fibo[fibo.length-2] + fibo[fibo.length-1]; //新しいフィボナッチ数を計算
  fibo = append(fibo, nextFibo);  //新しいフィボナッチ数を配列に加える
  drawSquare();
  println(nextFibo);
}
void draw(){}
