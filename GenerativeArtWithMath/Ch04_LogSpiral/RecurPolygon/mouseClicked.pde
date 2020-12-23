void mouseClicked(){
  gap = random(1) / 2;
  gon = int(random(4, 16));
  background(255);
  vec = new PVector[gon];
  for(int i = 0; i < gon; i++){ //正多角形の頂点の位置ベクトル
    vec[i] = PVector.fromAngle(2 * i * PI / gon);
    vec[i].mult(width / 2);
  }
}
