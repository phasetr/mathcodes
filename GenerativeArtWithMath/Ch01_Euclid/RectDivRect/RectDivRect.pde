//縦横比がnumB:numAの長方形を逆の比の長方形によって分割
int numA = 10;
int numB = 6;
float ratio = (float) numB / numA;
void setup(){ //最初に1度だけ実行する関数
  size(500, 500);
  colorMode(HSB, 1);
  //この関数内だけのローカル変数
  int itr = 0;
  float xPos = 0;
  float yPos = 0;
  float wd = width * ratio;
  while (wd > 0.1){
    itr++;
    if (itr % 2 == 1){
      while (xPos + wd < width + 0.1){
        divSquare(xPos, yPos, wd);  //正方形を分割する関数の呼び出し
        xPos += wd;
      }
      wd = width - xPos;
    } else {
      while (yPos + wd < width * ratio + 0.1){
        divSquare(xPos, yPos, wd);  //正方形を分割する関数の呼び出し
        yPos += wd;
      }
      wd = width * ratio - yPos;
    }
  }
}
