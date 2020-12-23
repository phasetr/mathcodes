void makeHexRhomb(){
  color[] col = new color[3];
  for (int i = 0; i < 3; i++){
    col[i] = color(random(1), 1, 1);  //ひし形の色
  }
  PVector[] v = new PVector[6];
  for (int i = 0; i < 6; i++){
    v[i] = PVector.fromAngle(2 * PI * i / 6); //正六角形の頂点
    v[i].mult(scalar / sqrt(3));
  }
  tile = createShape(GROUP);
  makeRhomb(v, col);  //正六角形の頂点から3つのひし形を作り，タイルグループに追加
}
