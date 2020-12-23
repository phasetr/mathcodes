//位置(xPos,yPos)にある1辺がwdの正方形を縦横比がa:bの長方形で分割する
void divSq(float xPos, float yPos, float wd){
  int itr = 0;
  float xEndPos = wd + xPos;
  float yEndPos = wd + yPos;
  setColor();
  rect(xPos, yPos, wd, wd);
  while (wd > thr){  //wdがしきい値以上の場合に処理を行う
    itr++;
    if (itr % 2 == 1){
      while (xPos + wd * ratio < xEndPos + 0.1){
        divRect(xPos, yPos, wd * ratio);  //長方形を分割する関数の呼び出し
        xPos += wd * ratio;
      }
      wd = xEndPos - xPos;
    } else {
      while (yPos + wd / ratio < yEndPos + 0.1){
        divRect(xPos, yPos, wd);  //長方形を分割する関数の呼び出し
        yPos += wd / ratio;
      }
      wd = yEndPos - yPos;
    }
  }
}
