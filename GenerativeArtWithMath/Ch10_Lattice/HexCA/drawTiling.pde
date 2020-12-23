void drawTiling(){
  for (int i = 0; i < num; i++){
    for (int j = 0; j < num; j++){
      tile.resetMatrix();
      tile.translate(lattice[i][j].x, lattice[i][j].y);  //タイルの位置を指定
      setTileColor(tile, i, j);
      shape(tile);  //タイルを描画
    }
  }
}
