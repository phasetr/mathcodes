void drawTiling(){
  for (int i = 0; i < lattice.length; i++){
    for (int j = 0; j < lattice[0].length; j++){
      tile.resetMatrix(); //変換を初期化
      //タイルを鏡映してから移動
      tile.translate(lattice[i][j].x, lattice[i][j].y); //格子点ベクトルによる移動
      tile.scale(pow(-1, j), 1);  //jが奇数のとき，タイルをy軸を中心に鏡映する
      tile.setFill(tileColor[i][j]);  //タイルの配色
      shape(tile);  //タイルの描画
    }
  }
}
