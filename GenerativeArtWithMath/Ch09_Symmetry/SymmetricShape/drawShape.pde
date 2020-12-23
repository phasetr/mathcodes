void drawShape(){
  translate(width / 2, height / 2);
  for(int j = 0; j < 2; j++){
    for(int k = 0; k < gon; k++){
      pushMatrix();
      scale(1, pow(-1, j));
      rotate(k * 2 * PI / gon);
      shape(crv);
      popMatrix();
    }
  }
}
