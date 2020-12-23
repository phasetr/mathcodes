float ratio = (sqrt(5) + 1) / 2;  //黄金数
float thr = 80;  //分割する大きさに関するしきい値
float thr2 = 0.5; //確率を決定するしきい値
void setup(){
  size(500, 500);
  colorMode(HSB, 1);
  colorRect(0, 0, width, width);
  divSquare(0, 0, width);
}
