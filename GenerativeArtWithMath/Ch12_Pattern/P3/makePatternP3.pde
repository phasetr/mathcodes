void makePatternP3(){
  tile = createShape(GROUP);
  float[] rand = new float[4];
  for (int i = 0; i < 4; i++){
    rand[i] = random(-1, 1);  //ベジエ曲線の制御点に関するランダム変数
  }
  for (int i = 0; i < 3; i++){
    PShape rhomb = makeRhomb(rand);
    rhomb.rotate(2 * PI * i / 3); //ひし形の120度回転
    tile.addChild(rhomb);
  }
  tile.setFill(color(random(1), 1, 1));
}
