void drawTiling(){ //タイリングを描画
  background(0, 0, 1);
  for (PVector[] vecArr: lattice){
    for (PVector vec : vecArr){
      tile.resetMatrix();
      tile.translate(vec.x, vec.y);  //タイルの位置を指定
      // tile.rotate(int(random(6)) * PI / 3);  //ランダムに回転させる
      shape(tile);  //タイルを描画
    }
  }
}
