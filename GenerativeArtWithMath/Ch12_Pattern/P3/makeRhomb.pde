PShape makeRhomb(float[] rand){ 
  PVector[] v = new PVector[2]; //ひし形の長い対角線の端点
  for (int i = 0; i < 2; i++){
    v[i] = PVector.fromAngle(2 * PI * i / 3);
    v[i].mult(scalar / sqrt(3));
  }
  PVector[] ctr = new PVector[4]; //ベジエ曲線の制御点
  for (int i = 0; i < 4; i++){
    ctr[i] = PVector.sub(v[(i + 1) % 2], v[i % 2]);
    ctr[i].rotate(rand[i] * PI / 3); //ランダムな回転によって制御点を取る
    ctr[i].add(v[i % 2]);
  }
  PShape rhomb = createShape();
  rhomb.beginShape(); //ひし形の端点をつなぐ２つのベジエ曲線の生成
  rhomb.vertex(v[0].x, v[0].y); //1番目の制御点
  rhomb.bezierVertex(ctr[0].x, ctr[0].y,  //2番目の制御点
    ctr[1].x, ctr[1].y, //3番目の制御点
    v[1].x, v[1].y); //4番目の制御点=次のベジエ曲線の1番目の制御点
  rhomb.bezierVertex(ctr[3].x, ctr[3].y,  //2番目の制御点
    ctr[2].x, ctr[2].y, //3番目の制御点
    v[0].x, v[0].y);  //4番目の制御点
  rhomb.endShape();
  return rhomb;
}
