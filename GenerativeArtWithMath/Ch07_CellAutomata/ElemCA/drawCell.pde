void drawCell(float y){
  float scalar = width * 0.5 / num; // セルの大きさ
  float x = (width - state.length * scalar) * 0.5; // セルのx座標
  y *= scalar;
  noStroke();
  for (int i = 0; i < state.length; i++){
    fill(0, 0, 1 - state[i]); //色相にセルの状態を割り当て
    rect(x, y, scalar, scalar); // セルの描画
    x += scalar; // x座標方向にセルをずらす
  }
}
