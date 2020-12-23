void drawTable(int[][] mtx, float x, float y, color c1, color c2){
  float posY = y * scalar;  //セルのy座標位置
  for(int i = 0; i < mtx.length; i++){
    float posX = x * scalar;  //セルのx座標位置
    for (int j = 0; j < mtx[0].length; j++){
      if(mtx[i][j] == 0){
        fill(c2); //成分が0ならば色c2でセルを塗る
      } else {
        fill(c1); //成分が1ならば色c1でセルを塗る
      }
      rect(posX, posY, scalar, scalar); //行列をセルとして書き出し
      posX += scalar; //セルの位置を更新
    }
    posY += scalar;
  }
}
