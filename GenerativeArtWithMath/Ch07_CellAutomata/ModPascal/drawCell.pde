void drawCell(float y){
  float scalar = (float) width / num; // セルの大きさ
  float x = (width - state.length * scalar) * 0.5; // セルのx座標
  y *= scalar;
  noStroke();
  for (int i = 0; i < state.length; i++){
    fill(state[i] * 1.0 / mod, state[i] * 1.0 / mod, 1); //色相にセルの状態を割り当て
    rect(x, y, scalar, scalar); // セルの描画
    x += scalar; // x座標方向にセルをずらす
  }
}
