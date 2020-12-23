void deformHex(){
  PVector[] v = new PVector[6];
  for (int i = 0; i < 6; i++){
    v[i] = PVector.fromAngle(2 * PI * i / 6);
    v[i].mult(scalar / sqrt(3));
  }
  tile = createShape();
  tile.beginShape();
  float[][] rand = new float[3][2];
  for (int i = 0; i < 3; i++){  //ベジエ曲線の制御点生成のための乱数
    rand[i][0] = random(-1, 1);
    rand[i][1] = random(-1, 1);
  }
  tile.vertex(v[0].x, v[0].y);  //1つ目の制御点
  for (int i =0; i < 6; i++){ //6つの辺をベジエ曲線にする
    PVector[] w = parameterizeIH02(v, i, rand);
    tile.bezierVertex(w[0].x, w[0].y, //2つ目の制御点
      w[1].x, w[1].y, //3つ目の制御点
      v[(i+1)%6].x, v[(i+1)%6].y);  //4つ目の制御点
  }
  tile.endShape(CLOSE);
}
