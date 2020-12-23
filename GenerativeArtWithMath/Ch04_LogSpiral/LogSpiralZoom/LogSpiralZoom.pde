float STEP = 2 * PI * 0.01; //曲線の精度
void setup(){
  size(500, 500);
  colorMode(HSB, 1);
}
void draw(){
  background(1,0,1);
  drawLogSpiral();  //対数らせんを描画
}
