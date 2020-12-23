void setTileColor(PShape t, int i, int j){
  t.setFill(color(state[i][j] * 1.0 / mod, state[i][j] * 1.0 / mod, 1));
}
