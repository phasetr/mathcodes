void drawSquare(PVector[] v){ //四角形を描く
  for(int i = 0; i < 4; i++){
    line(v[i].x,
      v[i].y,
      v[(i + 1) % 4].x,
      v[(i + 1) % 4].y); //ベクトルのxy座標の値を取りだし，線分を描く
  }
}
