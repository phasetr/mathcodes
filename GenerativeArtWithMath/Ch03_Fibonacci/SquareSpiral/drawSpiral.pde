void drawSpiral(){
  int[] SGN = {-1, 1, 1, -1};  //敷き詰める方向を決める符号
  float xPos = 0;
  float yPos = 0;
  float scalar = (float) width / (2 * fibo[fibo.length - 1]);  //拡大・縮小比率
  background(0, 0, 1);
  translate(width / 2 ,height / 2); //描画ウィンドウ中央に移動
  for(int i = 1; i < fibo.length - 1; i++){
    fill((0.1 * i) % 1, 1, 1);
    //正方形を描く方向を符号の配列に従って変える
    rect(scalar * xPos,
      scalar * yPos,
      scalar * SGN[(i+1) % 4] * fibo[i],  //符号が負の場合，逆方向に正方形を描画
      scalar * SGN[i % 4] * fibo[i]);
    //正方形の位置を符号の配列に従って変える
    if (i % 2 == 1){
      xPos += SGN[i % 4] * (fibo[i] + fibo[i + 1]);
    } else {
      yPos += SGN[i % 4] * (fibo[i] + fibo[i + 1]);
    }
  }
}
