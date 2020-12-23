void drawSpiral(){
  float xPos = 0;
  float yPos = 0;
  float scalar = (float) width / (2 * fibo[fibo.length-1]);  //拡大・縮小比率
  background(0, 0, 1);
  translate(width / 2 ,height / 2); //描画ウィンドウ中央に移動
  for(int i = 1; i < fibo.length - 1; i++){
    stroke(0, 0, 0);
    rect(scalar * xPos,
      scalar * yPos,
      scalar * SGN[(i+1) % 4] * fibo[i],
      scalar * SGN[i % 4] * fibo[i]);
    stroke(0, 1, 1);
    arc(scalar * (xPos + SGN[(i+1) % 4] * fibo[i]),  //円の中心のx座標
      scalar * (yPos + SGN[i % 4] * fibo[i]),  //円の中心のy座標
      scalar * 2 * fibo[i],  //楕円の縦の直径
      scalar * 2 * fibo[i],  //楕円の横の直径(正円のため縦と同じ)
      (1 + i) * PI / 2,  //円弧の開始位置(ラジアン)
      (2 + i) * PI / 2);  //円弧の終了位置
    if (i % 2 == 1){
      xPos += SGN[i % 4] * (fibo[i] + fibo[i+1]);
    } else {
      yPos += SGN[i % 4] * (fibo[i] + fibo[i+1]);
    }
  }
}
