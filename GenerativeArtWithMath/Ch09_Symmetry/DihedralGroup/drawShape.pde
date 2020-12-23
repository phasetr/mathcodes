void drawShape(){
  background(200);
  translate(width / 2, height / 2);
  img.resetMatrix();  //画像の変換を初期化
  img.scale(1, j);  //x軸に関する鏡映
  img.rotate(k * 2 * PI / gon); //回転
  shape(img);
  shape(polygon);
  for (int i = 0; i < gon; i++){
    int ind = (j * i - k + 2 * gon) % gon;  //頂点の番号付け
    PVector v = PVector.fromAngle(2 * PI *  i / gon);
    v.mult(scalar);
    textSize(20);
    text(ind, v.x, v.y);  //番号を表示
  }
}
