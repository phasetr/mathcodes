void makePythagoras(){
  PVector[] v = new PVector[9]; //正方形タイルの生成
  for (int i = 0; i < 4; i++){
    v[i] = PVector.fromAngle(2 * PI * (i + 0.5) / 4); //正方形の頂点
    v[i].mult(scalar / sqrt(2));
  }
  // gap = (sqrt(5) - 1) / 2; //黄金数の逆数
  float theta = atan(gap); //回転角
  PVector slope = PVector.sub(v[1],v[0]); //正方形タイルをずらす方向
  slope.rotate(theta);
  v[4] = slope.copy();
  v[4].mult(sin(theta));
  v[4].add(v[0]);
  v[5] = slope.copy();
  v[5].mult(cos(theta));
  v[5].add(v[0]);
  v[6] = slope.copy();
  v[6].mult(1.0 / cos(theta));
  v[6].add(v[0]);
  v[7] = PVector.sub(v[5], v[1]);
  v[7].add(v[4]);
  v[8] = PVector.sub(v[6], v[1]);
  v[8].add(v[0]);
  tile = createShape(GROUP);
  makeDoubleSq(v);  //2種類の正方形をグループに追加
  makeEdge(v);  //正方形の輪郭線をグループに追加
}
