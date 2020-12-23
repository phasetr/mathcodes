void makeSquare(){  //正方形タイルの生成
  tile = createShape();
  tile.beginShape();
  for (int i = 0; i < 4; i++){
    PVector v = PVector.fromAngle(2 * PI * (i + 0.5) / 4); //正方形の頂点を時計回りに設定
    v.mult(scalar / sqrt(2)); //正方形の対角線の長さの半分をかける
    tile.vertex(v.x, v.y);
  }
  tile.endShape(CLOSE);
}
