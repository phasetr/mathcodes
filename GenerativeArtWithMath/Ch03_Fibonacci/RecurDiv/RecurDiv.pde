int num = 10;
int thr = 1;  //関数の繰り返し回数に関するしきい値
int[] fibo;
int[] SGN = {1, 1, -1, -1};
void setup(){
  size(500, 500);
  colorMode(HSB, 1);
  background(0, 0, 1);
  generateFibo(num);  //num項目までのフィボナッチ数列を生成
  divSquare(0, 0, 0, 0, 1, 1);  //正方形のフィボナッチ分割
}
