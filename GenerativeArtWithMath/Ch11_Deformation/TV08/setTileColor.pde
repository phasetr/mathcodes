void setTileColor(){ //タイルごとに色をランダムに指定
  tileColor = new color[row + 1][col + 1];
  for (int i = 0; i < row + 1; i++){
    for (int j = 0; j < col + 1; j++){
      tileColor[i][j] = color(random(1), 1, 1);
    }
  }
}
