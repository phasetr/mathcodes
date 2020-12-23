float ratio = sqrt(2);
void setup(){
  size(500, 353); //描画ウィンドウの横縦比をsqrt(2):1に設定
  colorMode(HSB, 1);
}
void draw(){
  background(0, 0, 1);
  float scalar = pow(50, mouseX * 1.0 / width) * width;
  //マウスのカーソルのX座標によって長方形を1～50倍まで拡大する
  divRect(width - scalar, height - scalar / ratio, scalar);  //長方形を分割
}
