void mouseClicked() {
  int nextFibo = fibo[fibo.length-2] + fibo[fibo.length-1];
  fibo = append(fibo, nextFibo);
  drawSpiral();
  println(nextFibo);
}
void draw(){}
