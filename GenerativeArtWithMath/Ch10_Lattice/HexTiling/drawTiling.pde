void drawTiling(){ //タイリングを描画
  for (PVector[] vecArr: lattice){
    for (PVector vec : vecArr){
      tile.resetMatrix();
      tile.translate(vec.x, vec.y);  //タイルの位置を指定
      tile.setFill(color(random(1), 1, 1));
      shape(tile);  //タイルを描画
    }
  }
}
