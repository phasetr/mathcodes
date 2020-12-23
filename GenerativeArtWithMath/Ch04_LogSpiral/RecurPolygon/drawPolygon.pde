void drawPolygon(PVector[] v){
  for(int i = 0; i < gon; i++){
    line(v[i].x, v[i].y, v[(i + 1) % gon].x, v[(i + 1) % gon].y);
  }
}
