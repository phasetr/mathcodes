void makeSqTriangle(){
  PVector[] v = new PVector[4]; //正方形の頂点(点0,1,2,3)
  color col = color(random(1), 0.4, 1);
  // color[] col = new color[4];
  float gap = random(1); //正方形のずれ
  // float gap = 1.0 / (sqrt(3) + 1); //正三角形と正方形による半正則タイリング
  for (int i = 0; i < 4; i++){
    v[i] = PVector.fromAngle(PI * (i + 0.5) / 2);
    v[i].mult(0.5 * scalar / sqrt(2));
    // col[i] = color(random(1), 1, 1);
  }
  tile = createShape(GROUP);
  for (int i = 0; i < 2; i++){
    for (int j = 0; j < 2; j++){
      PShape quarter = makeTriangle(v, col, gap); //三角形を作る(正方形は背景色を利用)
      // PShape quarter = makePentagon(v, col, gap);  //五角形を作る
      //4分の1正方形を移動してから鏡映する
      quarter.scale(pow(-1, i), pow(-1, j));
      quarter.translate(scalar / 4, scalar / 4);
      tile.addChild(quarter);
    }
  }
}
