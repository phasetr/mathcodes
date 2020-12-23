int numA = 10;
int numB = 6;
int scalar = 50;
numA *= scalar;
numB *= scalar;
int wd = numB;
int xPos = 0;
int yPos = 0;
int itr = 0;
color col;  //色のための変数
//描画
size(500, 500);
colorMode(HSB, 1);  //01区間をパラメータとするHSB色形式を使用
//ループ
while (wd > 0) {
  itr++;
  if (itr % 2 ==1) {
    while (xPos + wd <= numA) {
      col = color(random(1), 1, 1);  //色相のみを01区間でランダムに変える
      fill(col);
      rect(xPos, yPos, wd, wd);
      xPos += wd;
    }
    wd = numA - xPos;
  } else {
    while (yPos + wd <= numB) {
      col = color(random(1), 1, 1);
      fill(col);
      rect(xPos, yPos, wd, wd);
      yPos += wd;
    }
    wd = numB - yPos;
  }
}
