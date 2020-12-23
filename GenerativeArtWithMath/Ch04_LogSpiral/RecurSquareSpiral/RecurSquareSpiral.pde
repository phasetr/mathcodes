PVector[] vec;  //PVectorクラスのインスタンスの配列を生成
float gap = 0.2;
void setup(){
  size(500, 500);
  vec = new PVector[4];
  vec[0] = new PVector(0, 0);
  vec[1] = new PVector(width, 0);
  vec[2] = new PVector(width, height);
  vec[3] = new PVector(0, height);
  drawLogSpiral();
}
void draw(){
  stroke(0);  //枠線を黒色にする
  strokeWeight(1);
  drawSquare(vec);
  vec = getVector(vec);
}
