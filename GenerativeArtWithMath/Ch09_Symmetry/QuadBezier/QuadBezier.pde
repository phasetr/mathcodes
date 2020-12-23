PVector[] ctr = new PVector[3]; //制御点
int step = 10;  //中間点の数(曲線の精度)
int itr = 0;  //繰り返し回数
void setup(){
  size(500, 500);
  colorMode(HSB, 1);
  ctr[0] = new PVector(0, 0);
  ctr[1] = new PVector(width, 0);
  ctr[2] = new PVector(width, height);
  noFill();
}
void draw(){
  PVector[] midPt = ctr;
  while(midPt.length > 1){  //中間点の個数が1個になるまで続ける
    midPt = getMidPoint(midPt, itr * 1.0 / step); //中間点を取る(点の個数が1つ減る)
    stroke(midPt.length * 1.0 / ctr.length, 1, 1);
    drawLine(midPt);  //中間点をつないで線分を作る
  }
  itr++;
  if(itr > step){
    stroke(0, 0, 0);
    strokeWeight(1);
    beginShape();
    vertex(0, 0);
    quadraticVertex(width, 0, width, height); //2次ベジエ曲線を描く関数
    endShape();
    noLoop(); //繰り返し処理の終了
  }
}
