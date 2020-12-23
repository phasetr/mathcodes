int num = 250;  // 行と列の長さ
int mod = 4;  // 法とする数
int[][] state = new int[num][num];  // セルの状態を表す行列
void setup(){
  size(500, 500);
  colorMode(HSB, 1);
  initialize();  // 初期化する
  frameRate(2);  // 0.5秒ごとに遷移
}
void draw(){
  drawCell();
  updateState();
}
