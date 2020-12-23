void makePatternP6(){
  tile = createShape(GROUP);
  float[] rand = new float[4];
  for (int i = 0; i < 4; i++){
    rand[i] = random(-1, 1);  //ベジエ曲線の制御点に関するランダム変数
  }
  for (int i = 0; i < 6; i++){
    PShape tri = makeTriangle(rand);
    tri.rotate(2 * PI * i / 6); //60°回転
    tile.addChild(tri);
  }
}
