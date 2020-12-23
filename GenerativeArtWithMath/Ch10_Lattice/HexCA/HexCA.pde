PVector[][] lattice;
PShape tile;
PVector[] base = new PVector[2];  //格子を張るベクトル
int num = 200;
float scalar;
int[][] state = new int[num][num];  //セルの状態を表す行列
int mod = 10; //法とする数
void setup(){
  size(693, 800);
  colorMode(HSB, 1);
  scalar = height * 1.0 / num;
  initialize();  // 初期状態
  makeHexVector(); //六角格子を張るベクトルの生成
  makeLattice();  //格子点ベクトルを生成
  makeHex();  //正六角形タイルを生成
  drawTiling(); //タイリングを描画
}
void draw(){
  background(0, 0, 1);
  int[][] nextState = new int[num][num]; //次世代の行列
  for (int i = 0; i < num; i++){
    for (int j = 0; j < num; j++){
      nextState[i][j] = transition(i, j); //遷移
    }
  }
  state = nextState;  //状態を更新
  drawTiling();
}
