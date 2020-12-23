int[] fibo = {0,1,1};
int[] SGN = {-1, 1, 1, -1};  //敷き詰める方向
void setup(){
  size(500, 500);
  colorMode(HSB, 1);
  drawSpiral();
}
