void makePatternP6M(){
  tile = createShape(GROUP);
  float[] rand = new float[4];
  for (int i = 0; i < 4; i++){
    rand[i] = random(-1, 1);  //ベジエ曲線の制御点に関するランダム変数
  }
  for (int i = 0; i < 2; i++){
    for (int j = 0; j < 6; j++){
      PShape tri = makeTriangle(rand);  //直角三角形上の曲線模様をランダムに生成
      tri.scale(1, pow(-1, i)); //x軸を中心に鏡映
      tri.rotate(2 * PI * j / 6); //60度回転
      tile.addChild(tri); //グループに追加
    }
  }
  tile.setFill(color(random(1), 1, 1));
}
