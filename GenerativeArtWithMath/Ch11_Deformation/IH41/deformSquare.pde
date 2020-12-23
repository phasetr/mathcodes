void deformSquare(){
  PVector[] v = new PVector[4];
  for (int i = 0; i < 4; i++){
    v[i] = PVector.fromAngle(2 * PI * (i + 0.5) / 4); //正方形の頂点
    v[i].mult(scalar / sqrt(2));
  }
  tile = createShape();
  tile.beginShape();
  float[][] rand = new float[2][2];
  for (int i = 0; i < 2; i++){  //ベジエ曲線の制御点生成のための乱数
    rand[i][0] = random(-1, 1);
    rand[i][1] = random(-1, 1);
  }
  tile.vertex(v[0].x, v[0].y);  //1つ目の制御点
  for (int i =0; i < 4; i++){ //4つの辺をベジエ曲線にする
    PVector[] w = parameterizeIH41(v, i, rand);  //制御点の生成
    tile.bezierVertex(w[0].x, w[0].y, //2つ目の制御点
      w[1].x, w[1].y, //3つ目の制御点
      v[(i + 1) % 4].x, v[(i + 1) % 4].y);  //4つ目の制御点
  }
  tile.endShape(CLOSE);
}
