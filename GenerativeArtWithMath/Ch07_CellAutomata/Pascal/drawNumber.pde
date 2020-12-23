void drawNumber(float y){
  float scalar = (float) width / num; // 数字の大きさ
  float x = (width - state.length * scalar) * 0.5; // 数字を書く位置のx座標
  y *= scalar;
  fill(0);
  for (int i = 0; i < state.length; i++){
    textSize(scalar * 0.5);
    text(state[i], x + scalar * 0.5, y + scalar * 0.5);
    x += scalar; // 数字を書く位置をx座標方向にずらす
  }
}
