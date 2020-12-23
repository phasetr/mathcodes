void makeDoubleSq(PVector[] v){
  int[][][] indDomain = { //正方形を構成する領域の頂点
    {{0, 1, 5}, {4, 6, 2, 3}, {3, 7, 8}}, //正方形1
    {{1, 5, 6}, {0, 4, 7, 8}} //正方形2
  };
  PShape[] sq = new PShape[2];  //2つの正方形グループ
  for (int i = 0; i < 2; i++){
    sq[i] = createShape(GROUP);
    for (int[] ind : indDomain[i]){
      PShape elm = createShape(); //正方形のかけら
      elm.setFill(col[i]);
      elm.beginShape();
      elm.noStroke();
      for (int j : ind){
        elm.vertex(v[j].x, v[j].y); //正方形のかけらの頂点
      }
      elm.endShape(CLOSE);
      sq[i].addChild(elm);  //かけらを正方形グループに追加
    }
    tile.addChild(sq[i]); //正方形グループをタイルグループに追加
  }
}
