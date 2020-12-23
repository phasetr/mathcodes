void divRect(float xPos, float yPos, float wd){  //長方形を分割する関数
  int itr = 0;
  float xEndPos = xPos + wd;  //長方形の横の長さ
  float yEndPos = yPos + wd / ratio;   //長方形の縦の長さ
  while (wd > thr){   //wdがしきい値以上の場合に処理を行う
    itr++;
    if (itr % 2 == 0){
      while (xPos + wd < xEndPos + 0.1){
        colorRect(xPos, yPos, wd, wd);  //正方形を描く
        if (random(1) < thr2){
          divSquare(xPos, yPos, wd);  //正方形を分割する関数の呼び出し
        }
        xPos += wd;
      }
      wd = xEndPos - xPos;
    } else {
      while (yPos + wd < yEndPos + 0.1){
        colorRect(xPos, yPos, wd, wd);  //正方形を描く
        if (random(1) < thr2){
          divSquare(xPos, yPos, wd);  //正方形を分割する関数の呼び出し
        }
        yPos += wd;
      }
      wd = yEndPos - yPos;
    }
  }
}

void divSquare(float xPos, float yPos, float wd){  //正方形を分割する関数
  int itr = 0;
  float xEndPos = wd + xPos;  //正方形の横の長さ
  float yEndPos = wd + yPos;  //正方形の縦の長さ
  while (wd > thr){  //正方形の幅がしきい値以上の場合に実行
    itr++;
    if (itr % 2 ==1){
      while (xPos + wd * ratio < xEndPos + 0.1){
        colorRect(xPos, yPos, wd * ratio, wd);  //長方形を描く
        if (random(1) < thr2){  //thr2の確率で再分割
          divRect(xPos, yPos, wd * ratio);  //長方形を分割する関数の呼び出し
        }
        xPos += wd * ratio;
      }
      wd = xEndPos - xPos;
    } else {
      while (yPos + wd / ratio < yEndPos + 0.1){
        colorRect(xPos, yPos, wd, wd / ratio);  //長方形を描く
        if (random(1) < thr2){  //thr2の確率で再分割
          divRect(xPos, yPos, wd);  //長方形を分割する関数の呼び出し
        }
        yPos += wd / ratio;
      }
      wd = yEndPos - yPos;
    }
  }
}
