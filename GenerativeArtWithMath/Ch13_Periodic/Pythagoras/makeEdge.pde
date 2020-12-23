void makeEdge(PVector[] v){
  PShape lin;
  int[][] indLine = {{0, 6}, {1, 5}, {3, 4}, {7, 8}}; //辺の頂点
  lin = createShape(GROUP);
  for (int[] ind : indLine){
    PShape elm = createShape(); //辺のかけら
    elm.beginShape();
    for (int i : ind){
      elm.vertex(v[i].x, v[i].y); //辺のかけらの頂点
    }
    lin.addChild(elm);  //かけらを辺のグループに追加
  }
  tile.addChild(lin); //辺のグループをタイルグループに追加
}
