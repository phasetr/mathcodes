int rowA = 20;  //Aの行数(Cの列数)
int columnA = 4; //A,Bの列数(B,Cの行数)
int[][] mtxA = new int[rowA][columnA];
int[][] mtxB = new int[columnA][columnA];
int[][] mtxC = new int[columnA][rowA];
int[][] mtxP = new int[rowA][rowA];
float scalar;
color colorTate = color(255, 255, 0);  //タテ糸の色(黄)
color colorYoko = color(255, 0, 0);  //ヨコ糸の色(赤)
color BLACK = color(0, 0, 0);  //黒
color WHITE = color(255, 255, 255); //白
void setup(){
  size(500, 500);
  initialize(mtxA);  //Aを初期化する
  initialize(mtxB);  //Bを初期化する
  initialize(mtxC);  //Cを初期化する
  scalar = (float) height / (rowA + columnA); // セルのサイズ
}
void draw(){
  mtxP = multMtx(multMtx(mtxA, trMtx(mtxB)), mtxC); //Pの計算
  strokeWeight(1);
  drawTable(mtxA, 0, columnA, BLACK, WHITE);  //Aを表に書き出す(1が黒，0が白)
  drawTable(mtxB, 0, 0, BLACK, WHITE);  //Bを表に書き出す
  drawTable(mtxC, columnA, 0, BLACK, WHITE);  //Cを表に書き出す
  drawTable(mtxP, columnA, columnA, colorYoko, colorTate);//Pを表に書き出す(1がヨコ糸，0がタテ糸)
  strokeWeight(3);
  line(0, scalar * columnA, width, scalar * columnA); //罫線の描画
  line(scalar * columnA, 0, scalar * columnA, height);
}
