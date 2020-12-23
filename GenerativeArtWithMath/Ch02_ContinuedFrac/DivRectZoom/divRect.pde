void divRect(float xPos, float yPos, float wd){
  int itr = 0;
  float xEndPos = xPos + wd;
  float yEndPos = yPos + wd / ratio;
  while (wd > 0.1){
    itr++;
    fill(color((itr * ratio) % 1, 1, 1));  //色を指定
    if (itr % 2 == 0){
      while (xPos + wd < xEndPos + 0.1){
        rect(xPos, yPos, wd, wd);
        xPos += wd;
      }
      wd = xEndPos - xPos;
    } else {
      while (yPos + wd < yEndPos + 0.1){
        rect(xPos, yPos, wd, wd);
        yPos += wd;
      }
      wd = yEndPos - yPos;
    }
  }
}
