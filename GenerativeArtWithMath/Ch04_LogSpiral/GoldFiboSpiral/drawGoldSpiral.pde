void drawGoldSpiral(){
  float scalar = (float) width / (2 * fibo[fibo.length - 1]);
  float PHI = (1 + sqrt(5)) / 2;  //黄金数
  float STEP = -PI / 50;
  PVector O = new PVector(1, 1);  //らせんの中心
  PVector v = new PVector(0, 1);  //らせんの出発点
  for(int i = 1; i < fibo.length - 1; i++){
      v.add(SGN[i % 4]* fibo[i], SGN[(i-1) % 4]* fibo[i]);  //出発点を順に移動
  }
  v.sub(O);
  v.mult(scalar); //ウィンドウサイズに合わせてスカラー倍
  translate(scalar,  scalar); //ウィンドウサイズに合わせて移動
  for (int i = 0; i < (fibo.length - 2) * 25; i++){ //正方形1つにつき90度分のらせんを描画
    PVector nextV = v.copy();
    nextV.rotate(STEP);
    nextV.mult(pow(PHI, 2 * STEP / PI));
    line(v.x, v.y, nextV.x, nextV.y);
    v = nextV;
  }
}
