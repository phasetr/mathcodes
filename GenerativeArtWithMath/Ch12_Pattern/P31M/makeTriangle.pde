PShape makeTriangle(float[] rand, color col1, color col2){  //正三角形部分の模様
  PShape tri = createShape(GROUP);  //正三角形を構成するグループ
  for (int i = 0; i < 3; i++){
    PShape pat = makeLine(rand);  //直線模様の生成
    pat.setFill(col1);
    pat.rotate(2 * PI * i / 3); //120度回転
    tri.addChild(pat);  //グループに追加
  }
  for (int i = 0; i < 3; i++){
    PShape pat = makeCurve(rand);  //曲線模様の生成
    pat.setFill(col2);
    pat.rotate(2 * PI * i / 3); //120度回転
    tri.addChild(pat);  //グループに追加
  }
  PVector v = PVector.fromAngle(-PI / 6);
  v.mult(scalar / 3);
  tri.translate(v.x, v.y);  //模様の位置をずらす
  return tri;
}
