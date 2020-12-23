void drawSquare(){
  float xPos = 0;  //正方形のx位置
  float yPos = 0;  //正方形のy位置
  float nextFibo = fibo[fibo.length-2] + fibo[fibo.length-1]; //次のフィボナッチ数
  float scalar = (float) width / nextFibo;  //長方形がウィンドウ幅に収まるように拡大
  background(0, 0, 1);  //描画ごとに背景を白く塗りつぶし
  for(int i = 1; i < fibo.length; i++){
    fill((0.1 * i) % 1, 1, 1);
    rect(scalar * xPos,
      scalar * yPos,
      scalar * fibo[i],
      scalar * fibo[i]);
    //正方形の位置は順にフィボナッチ数を足す・引くことで移動させる
    if (i % 2 == 1){  //数列の順番に従って交互に符号を変える
      xPos += fibo[i];
      yPos -= fibo[i-1];
    } else {
      xPos -= fibo[i-1];
      yPos += fibo[i];
    }
  }
}
