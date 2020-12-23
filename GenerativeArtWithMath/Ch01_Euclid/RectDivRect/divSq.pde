//位置(xPos,yPos)にある1辺がwdの正方形を縦横比がnumA:numBの長方形で分割する
void divSquare(float xPos, float yPos, float wd){
  //この関数内だけのローカル変数
  int itr = 0;
  float xEndPos = wd + xPos;  //正方形の右下の頂点のx座標
  float yEndPos = wd + yPos;  //正方形の右下の頂点のy座標
  //繰り返し処理
  while (wd > 0.1){
    itr++;
    if (itr % 2 == 1){
      while (xPos + wd * ratio < xEndPos + 0.1){  //ratioはグローバル変数
        fill(color(random(1), 1, 1));
        rect(xPos, yPos, wd * ratio, wd);
        xPos += wd * ratio;
      }
      wd = xEndPos - xPos;
    } else {
      while (yPos + wd / ratio < yEndPos + 0.1){
        fill(color(random(1), 1, 1));
        rect(xPos, yPos, wd, wd / ratio);
        yPos += wd / ratio;
      }
      wd = yEndPos - yPos;
    }
  }
}
