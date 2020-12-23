int num = 5;
PVector[] ctr = new PVector[num];
int step = 30;
int itr = 0;
void setup(){
  size(500, 500);
  colorMode(HSB, 1);
  for(int i = 0; i < num; i++){
    ctr[i] = PVector.random2D();  //単位円内にランダムにベクトルを取る関数
    ctr[i].mult(width / 2);
    ctr[i].add(width / 2, height / 2);
  }
  noFill();
}
void draw(){
  if(itr == 0){
    background(0, 0, 1);
  }
  PVector[] midPt = ctr;
  while(midPt.length > 1){
    midPt = getVertex(midPt, itr * 1.0 / step);
    stroke(midPt.length * 1.0 / ctr.length, 1, 1, 0.2);
    drawLine(midPt);
  }
  itr++;
  if(itr > step){
    noLoop();
  }
}
void mouseClicked(){
  itr = 0;
  for(int i = 0; i < num; i++){
    ctr[i] = PVector.random2D();
    ctr[i].mult(width / 2);
    ctr[i].add(width / 2, height / 2);
  }
  loop();
}
