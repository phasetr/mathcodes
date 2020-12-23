void drawRect(){
  int[] SGN = {-1, 1, 1, -1};  //敷き詰める方向
  float xPos = 0;
  float yPos = 0;
  float scalar = (float) width / (2 * fibo[fibo.length - 1]);  //拡大・縮小比率
  background(0, 0, 1);
  translate(width / 2 ,height / 2); //描画ウィンドウ中央に移動
  for(int i = 1; i < fibo.length - 1; i++){
    fill((0.1 * i) % 1, 1, 1);
    rect(scalar * xPos,
      scalar * yPos,
      scalar * SGN[(i+1) % 4] * fibo[i-1],  //横が短辺
      scalar * SGN[i % 4] * fibo[i]);  //縦が長辺(次の項のフィボナッチ数)
    if (i % 2 == 1){
      xPos += SGN[i % 4] * (fibo[i-1] + fibo[i]);  //x位置の取り方を変更
    } else {
      yPos += SGN[i % 4] * (fibo[i] + fibo[i+1]);
    }
  }
}
