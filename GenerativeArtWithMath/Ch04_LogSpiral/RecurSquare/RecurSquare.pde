PVector[] vec;  //PVector型の配列を宣言
float gap = 0.01;  //内接する正方形のずれ
void setup(){
  size(500, 500);
  vec = new PVector[4]; //4のベクトルを生成
  vec[0] = new PVector(0, 0); //ウィンドウ左上の角
  vec[1] = new PVector(width, 0); //ウィンドウ右上の角
  vec[2] = new PVector(width, height);  //ウィンドウ右下の角
  vec[3] = new PVector(0, height);  //ウィンドウ左下の角
}
void draw(){
  drawSquare(vec);  //4つのベクトルを頂点とする四角形を描画
  vec = getVector(vec); //ベクトルをgapの分だけずらす
}
