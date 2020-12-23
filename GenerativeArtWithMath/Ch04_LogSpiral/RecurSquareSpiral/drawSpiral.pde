void drawLogSpiral(){
  float STEP = 2 * PI * 0.001;
  float b = sqrt(2 * gap * gap - 2 * gap + 1);
  float c = atan(gap / (1 - gap));
  PVector O = new PVector(width / 2, height / 2); //ウィンドウの中心
  PVector v = new PVector(0, 0);  //ウィンドウの左上の角
  v.sub(O);
  translate(O.x, O.y);
  stroke(color(255, 0, 0));
  strokeWeight(3);
  while(v.mag() > 1){ //ベクトルの長さが1以下になれば停止
    PVector nextV = v.copy(); //ベクトルをコピーして新たなベクトルを生成
    nextV.rotate(STEP); //ベクトルの回転
    nextV.mult(pow(b, STEP / c));  //ベクトルのスカラー倍
    line(v.x, v.y, nextV.x, nextV.y);
    v = nextV;
  }
}
