float theta = 0;
float STEP = 2 * PI * 0.01; //曲線の精度
void setup(){
  size(500, 500);
}
void draw(){
  translate(width / 2, height / 2);  //描画ウィンドウの中心に移動
  line(rad(theta) * cos(theta),
   rad(theta) * sin(theta),
   rad(theta + STEP) * cos(theta + STEP),
   rad(theta + STEP) * sin(theta + STEP));
  theta += STEP;
}
float rad(float t){ //動径を定める関数
  float r = 5 * t;  //アルキメデスらせん
  // float r = 20 * sqrt(t); //フェルマーらせん
  // float r = pow(1.1, t); //対数らせん
  return(r);
}
