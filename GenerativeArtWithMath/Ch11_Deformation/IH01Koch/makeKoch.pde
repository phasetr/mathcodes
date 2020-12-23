void makeKoch(PVector startPt, PVector endPt, int itr, boolean conv){
  if (itr == upperLimit || itr > 5){
    tile.vertex(startPt.x, startPt.y);
    tile.vertex(endPt.x, endPt.y);
    return;
  }
  PVector[] v = new PVector[5];
  PVector dir = PVector.sub(endPt, startPt);
  dir.mult(1.0 / 3);
  PVector slope = dir.copy();
  if (conv){  //コッホ曲線の凹凸の設定
    slope.rotate(PI / 3);  //始点から終点に向かって時計回りに凸
  } else {
    slope.rotate(-PI / 3);  //始点から終点に向かって反時計回りに凸
  }
  v[0] = startPt;
  v[1] = PVector.add(startPt, dir);
  v[2] = PVector.add(v[1], slope);
  v[3] = PVector.sub(endPt, dir);
  v[4] = endPt;
  itr++;
  for (int i = 0; i < 4; i++){
    makeKoch(v[i], v[i+1], itr, conv);
  }
}
