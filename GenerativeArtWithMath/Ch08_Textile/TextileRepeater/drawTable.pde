void drawTable(int[][] mtx, float x, float y, color c1, color c2){
  noStroke();
  float posY = y * scalar;
  for(int i = 0; i < mtx.length; i++){
    float posX = x * scalar;
    for (int j = 0; j < mtx[0].length; j++){
      if(mtx[i][j] == 0){
        fill(c2);
      } else {
        fill(c1);
      }
      rect(posX, posY, scalar, scalar);
      posX += scalar;
    }
    posY += scalar;
  }
}
