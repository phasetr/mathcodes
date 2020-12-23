void drawLogSpiral(){
  float theta = 0;
  float scalar = pow(10, (float) mouseX / width) * height / 2;
  //マウスのx座標によって1～10倍に拡大する
  translate(width / 2, height / 2);  //描画ウィンドウの中心に移動
  for(int i = 0; i < 2000; i++){
    line(scalar * rad(theta) * cos(theta),
      scalar * rad(theta) * sin(theta),
      scalar * rad(theta + STEP) * cos(theta + STEP),
      scalar * rad(theta + STEP) * sin(theta + STEP));
    theta -= STEP;  //反時計回りに進むほど動径は減少する
  }
}
float rad(float t){ //動径を定める関数
  float r = pow(1.1, t);
  return(r);
}
