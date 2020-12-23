PVector[] vec;  //PVector型の配列を宣言
float gap = 0.1;  //内接する正多角形のずれ
int gon = 8;  //正多角形の頂点の数
void setup(){
  size(500, 500);
  vec = new PVector[gon];
  for(int i = 0; i < gon; i++){ //正多角形の頂点の位置ベクトル
    vec[i] = PVector.fromAngle(2 * i * PI / gon);
    vec[i].mult(width / 2);
  }
}
void draw(){
  translate(width / 2, height / 2); //描画ウィンドウの中心に移動
  drawPolygon(vec);
  vec = getVector(vec);
}
