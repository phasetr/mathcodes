int[] fibo = {0, 1, 1}; //フィボナッチ数列の配列
int[] SGN = {-1, 1, 1, -1};  //正方形を敷き詰める方向
void setup(){
  size(500, 500);
  translate(width / 2, height / 2);
  stroke(0);
  drawFiboSpiral(); //フィボナッチらせんの描画
  stroke(255, 0, 0);
  drawGoldSpiral(); //黄金らせんの描画
}
