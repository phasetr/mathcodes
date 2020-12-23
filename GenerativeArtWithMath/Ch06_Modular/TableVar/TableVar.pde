int mod = 7;  //法とする自然数
size(500, 500);
colorMode(HSB, 1);
background(0, 0, 1);
float scalar = (float) width / mod;
for (int i = 0; i < mod; i++){
  for (int j = 0; j < mod; j++){
    int num = (i + j) % mod;  //数の計算
    PVector v = new PVector(j + 0.5, i + 0.5);  //円の中心位置
    v.mult(scalar);
    // int num = (i * j) % mod;  //乗法表の場合
    // 色相に対応
    fill(num * 1.0 / mod, 1, 1);  //数を円の色相に対応
    noStroke();
    ellipse(v.x, v.y, scalar / 2, scalar / 2);
    // 円の大きさに対応
    fill(0, 0, 0);
    ellipse(v.x, v.y, scalar * num / mod, scalar * num / mod);  //数を円の直径に対応
  }
}
