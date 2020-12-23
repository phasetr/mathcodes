void drawTiling(){ //タイリングを描画
  for (PVector[] vecArr: lattice){
    for (PVector vec : vecArr){
      tile.resetMatrix();
      tile.translate(vec.x, vec.y);  //タイルの位置を指定
      for (int i = 0; i < tile.getChildCount(); i++){  //グループの個数だけ繰り返す
        PShape elm = tile.getChild(i); //グループの要素を取得
        elm.setFill(color(random(1), 1, 1));
      }
      shape(tile);  //タイルを描画
    }
  }
}
