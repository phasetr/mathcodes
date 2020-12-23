int mod = 37;
size(500, 500);
colorMode(HSB, 1);
background(0, 0, 1);
float scalar = (float) width / (mod - 1);
int num;
for (int i = 1; i < mod; i++){
  num = i;
  for (int j = 1; j < mod; j++){
    PVector v = new PVector(j - 0.5, i - 0.5);  //円の中心位置
    v.mult(scalar);
    // 色相に対応
    fill(num * 1.0 / mod, 1, 1);
    noStroke();
    ellipse(v.x, v.y, scalar / 2, scalar / 2);
    // 円の大きさに対応
    fill(0, 0, 0);
    ellipse(v.x, v.y, scalar * num / mod, scalar * num / mod);
    num = (num * i) % mod;  //numをiの(j+1)乗に更新
  }
}
