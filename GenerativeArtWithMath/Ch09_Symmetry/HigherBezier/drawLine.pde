void drawLine(PVector[] v){
  if(v.length > 1){
    for (int i = 0; i < v.length - 1; i++){
      strokeWeight(1);
      line(v[i].x, v[i].y, v[i + 1].x, v[i + 1].y);
    }
  } else {
    stroke(0, 0, 0);
    strokeWeight(8);
    point(v[0].x, v[0].y);
  }
}
