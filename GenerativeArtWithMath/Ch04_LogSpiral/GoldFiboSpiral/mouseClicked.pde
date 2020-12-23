void draw(){}
void mouseClicked() {
  background(255);
  int nextFibo = fibo[fibo.length-2] + fibo[fibo.length-1];
  fibo = append(fibo, nextFibo);
  translate(width / 2, height / 2);
  stroke(0);
  drawFiboSpiral();
  stroke(255, 0, 0);
  drawGoldSpiral();
  println(nextFibo);
}
