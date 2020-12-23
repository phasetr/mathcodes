void drawCell(){
  float scalar = (float) height / num; // セルのサイズ
  float y = 0;  // セルのy座標
  float x;
  for (int i = 0; i < num; i++){
    x = 0;  // セルのx座標
    for (int j = 0; j < num; j++){
      noStroke();
      fill(state[i][j] * 1.0 / mod, state[i][j] * 1.0 / mod, 1);  // セルの色
      rect(x, y, scalar, scalar);
      x += scalar;
    }
    y += scalar;
  }
}
