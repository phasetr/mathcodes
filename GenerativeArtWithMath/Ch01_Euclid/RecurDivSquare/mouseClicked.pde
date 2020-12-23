void mouseClicked(){
  numA = int(random(1, 20));  //1以上20以下のランダムな整数を代入
  numB = int(random(1, 20));
  while (numA == numB){ //numAとnumBが異なるようにする
    numB = int(random(1, 20));
  }
  thr = int(random(10,300));
  println("numA =", numA, "numB =", numB,"thr =", thr);  //numA,numB,thrの値を表示
  ratio = (float) numA / numB;
  background(0, 0, 1);  //背景を白で消去
  divSquare(0, 0, width);
}
void draw(){} //プログラムを実行している間，繰り返し実行する関数
