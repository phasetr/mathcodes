int numA = 10;
int numB = 6;
float ratio = (float) numB / numA;
float thr = 160;  //しきい値
void setup(){
  size(500, 500);
  colorMode(HSB, 1);
  divSquare(0, 0, width); //正方形の分割
}
