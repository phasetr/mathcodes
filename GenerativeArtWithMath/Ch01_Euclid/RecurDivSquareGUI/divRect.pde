//位置(xPos,yPos)にある横幅wdで縦横比がa:bの長方形を正方形によって分割する
void divRect(float xPos, float yPos, float wd){
  int itr = 0;
  float xEndPos = xPos + wd;
  float yEndPos = yPos + wd / ratio;
  setColor();
  rect(xPos, yPos, wd, wd / ratio);
  while (wd > thr){   //wdがしきい値以上の場合に処理を行う
    itr++;
    if (itr % 2 == 0){
      while (xPos + wd < xEndPos + 0.1){
        divSq(xPos, yPos, wd);  //正方形を分割する関数の呼び出し
        xPos += wd;
      }
      wd = xEndPos - xPos;
    } else {
      while (yPos + wd < yEndPos + 0.1){
        divSq(xPos, yPos, wd);  //正方形を分割する関数の呼び出し
        yPos += wd;
      }
      wd = yEndPos - yPos;
    }
  }
}
