void draw(){}
void mouseClicked(){
  num = int(random(2, 10));
  thr = int(random(0, 9));
  println("num =", num, "thr =", thr);
  background(0, 0, 1);
  generateFibo(num);
  divSquare(0, 0, 0, 0, 1, 1);
}
