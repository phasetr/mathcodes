void randomize(int[][] mtx){
  for (int i = 0; i < mtx.length; i++){
    for (int j = 0 ; j < mtx[0].length; j++){
      mtx[i][j] = int(random(2));  //ランダムな01配列の生成
    }
  }
  if(sym){  //対称行列にする場合
    for (int i = 0; i < mtx.length; i++){
      for (int j = i ; j < mtx[0].length; j++){
        mtx[j][i] = mtx[i][j];  //(i,j)成分と(j,i)成分が同じになるようにする
      }
    }
  }
  colorTate = color(random(1), 1, 1);  //色彩をランダムに生成
  colorYoko = color(random(1), 1, 1);
}
