void makePatternP3M1(){
  tile = createShape(GROUP);  //PShapeのグループを作る
  // float gap = random(0.01, 0.5); //再帰的な正三角形を作るパラメータ
  for (int i = 0; i < 2; i++){
    for (int j = 0; j < 3; j++){
      PShape tri = makeTriangle();  //正三角形上の模様の読み込み
      // PShape tri = makeRecurTriangle(gap);  //再帰的な正三角形の生成
      tri.scale(1, pow(-1, i)); //x軸を中心に鏡映
      tri.rotate(2 * PI * j / 3); //120度回転
      tile.addChild(tri); //グループに追加
    }
  }
}
