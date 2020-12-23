int columnA = 10; //A,Bの列数(B,Cの行数)
int rep = 10; //反復回数
int rowA = rep * columnA;  //Aの行数(Cの列数)
int[][] mtxA = new int[rowA][columnA];
int[][] mtxB = new int[columnA][columnA];
int[][] mtxC = new int[columnA][rowA];
int[][] mtxP = new int[rowA][rowA];
float scalar;
color colorTate = color(255, 255, 0);  //タテ糸の色(黄)
color colorYoko = color(255, 0, 0);  //ヨコ糸の色(赤)
color BLACK = color(0, 0, 0);  //黒
color WHITE = color(255, 255, 255); //白
boolean sym = true; //Bの鏡映対称性(trueならばD4，falseならばD2)
void setup(){
  size(500, 500);
  colorMode(HSB, 1);
  repeat(mtxA);  //Aを反復配列にする
  randomize(mtxB);  //Bをランダムな配列にする
  mtxC = trMtx(mtxA);  //CをAの転置とする
  scalar = (float) height / (rowA + columnA); // セルのサイズ
}
void draw(){
  mtxP = multMtx(multMtx(mtxA, trMtx(mtxB)), mtxC); //Dの計算
  strokeWeight(1);
  drawTable(mtxA, 0, columnA, BLACK, WHITE);  //Aを描画する
  drawTable(mtxB, 0, 0, BLACK, WHITE);  //Bを描画する
  drawTable(mtxC, columnA, 0, BLACK, WHITE);  //Cを描画する
  drawTable(mtxP, columnA, columnA, colorYoko, colorTate);    //Dを描画する
  strokeWeight(3);
  line(0, scalar * columnA, width, scalar * columnA); //罫線の描画
  line(scalar * columnA, 0, scalar * columnA, height);
}
