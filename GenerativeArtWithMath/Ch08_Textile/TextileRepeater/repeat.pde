void repeat(int[][] mtx){
  for (int i = 0; i < rowA; i++){
    for (int j = 0; j < columnA; j++){
      mtx[i][j] = 0;  //mtxの要素をすべて0にする
    }
  }
  for (int i = 0; i < rowA; i++){
    int iZigzag;
    if (int(i / columnA) % 2 == 0){
      iZigzag = i % columnA;
    } else {  //列の端まで達したとき折り返す
      iZigzag = columnA - (i % columnA) - 1;
    }
    mtx[i][iZigzag] = 1;  //行列の要素をジグザグに1にする
  }
}
