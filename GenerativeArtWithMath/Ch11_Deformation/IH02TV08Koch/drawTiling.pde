void drawTiling(){
  for (int i = 0; i < lattice.length; i++){
    for (int j = 0; j < lattice[0].length; j++){
      pushMatrix();
      translate(lattice[i][j].x, lattice[i][j].y);
      scale(pow(-1, j), 1);
      tile.setFill(tileColor[i][j]);
      shape(tile);
      popMatrix();
    }
  }
}
