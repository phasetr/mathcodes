void makeKoch(PVector startPt, PVector endPt, int itr){
  if (itr == upperLimit || itr > 5){ //繰り返しの上限を超えた場合は線分を描画
    crv.vertex(startPt.x, startPt.y);
    crv.vertex(endPt.x, endPt.y);
    return;
  }
  PVector[] v = new PVector[5];
  PVector dir = PVector.sub(endPt, startPt);  //始点から終点へ向かう方向
  dir.mult(1.0 / 3);
  PVector slope = dir.copy();
  slope.rotate(PI / 3); //三角形の頂点への方向
  v[0] = startPt; //始点
  v[1] = PVector.add(startPt, dir); //始点に近い山のふもとの点
  v[2] = PVector.add(v[1], slope);  //山頂の点
  v[3] = PVector.sub(endPt, dir); //終点に近い山のふもとの点
  v[4] = endPt; //終点
  itr++;
  for (int i = 0; i < 4; i++){
    makeKoch(v[i], v[i+1], itr);  //再分割
  }
}
