PVector[] ctr = new PVector[4];
int step = 10;
int itr = 0;
void setup(){
  size(500, 500);
  colorMode(HSB, 1);
  ctr[0] = new PVector(0, 0);
  ctr[1] = new PVector(width, 0);
  ctr[2] = new PVector(width, height);
  ctr[3] = new PVector(0, height);
  noFill();
}
void draw(){
  PVector[] midPt = ctr;
  while(midPt.length > 1){
    midPt = getMidPoint(midPt, itr * 1.0 / step);
    stroke(midPt.length * 1.0 / ctr.length, 1, 1);
    drawLine(midPt);
  }
  itr++;
  if(itr > step){
    stroke(0, 0, 0);
    strokeWeight(1);
    beginShape();
    vertex(0, 0);
    bezier(ctr[0].x, ctr[0].y, ctr[1].x, ctr[1].y, ctr[2].x, ctr[2].y, ctr[3].x, ctr[3].y);
    endShape();
    noLoop();
  }
}
