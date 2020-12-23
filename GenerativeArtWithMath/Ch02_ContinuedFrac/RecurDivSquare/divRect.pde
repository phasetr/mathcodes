//位置(xPos,yPos)にある横幅wdで縦横比がnumA:numBの長方形を正方形によって分割する
void divRect(float xPos, float yPos, float wd){
  int itr = 0;
  float xEndPos = xPos + wd;
  float yEndPos = yPos + wd / ratio;
  fill(color(random(1), 1, 1));
  rect(xPos, yPos, wd, wd / ratio);
  while (wd > thr){   //長方形の幅がしきい値以上の場合に処理を行う
    itr++;
    if (itr % 2 == 0){
      while (xPos + wd < xEndPos + 0.1){
        divSquare(xPos, yPos, wd);  //正方形を分割する関数の呼び出し
        xPos += wd;
      }
      wd = xEndPos - xPos;
    } else {
      while (yPos + wd < yEndPos + 0.1){
        divSquare(xPos, yPos, wd);  //正方形を分割する関数の呼び出し
        yPos += wd;
      }
      wd = yEndPos - yPos;
    }
  }
}
