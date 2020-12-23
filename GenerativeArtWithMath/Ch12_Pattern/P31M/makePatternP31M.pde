void makePatternP31M(){
  tile = createShape(GROUP);
  float[] rand = new float[4];
  for (int i = 0; i < 4; i++){
    rand[i] = random(-1, 1);  //模様のためのランダム変数
  }
  color col1 = color(random(1), 1, 1);  //直線模様のためのカラー変数
  color col2 = color(random(1), 1, 1);  //曲線模様のためのカラー変数
  for (int i = 0; i < 2; i++){
    for (int j = 0; j < 3; j++){
      PShape tri = makeTriangle(rand, col1, col2);
      tri.scale(1, pow(-1, i)); //x軸を中心に鏡映
      tri.rotate(2 * PI * j / 3); //120度回転
      tile.addChild(tri);
    }
  }
}
