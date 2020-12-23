int mod = 7;
size(500, 500);
float scalar = (float) width / (mod - 1);
int num;
for (int i = 1; i < mod; i++){
  num = i;  //iの1乗
  for (int j = 1; j < mod; j++){
    PVector v = new PVector(j - 1, i - 1);  //マスの位置
    v.mult(scalar);
    fill(255);
    rect(v.x, v.y, scalar, scalar); //マスを描画
    fill(0);
    textSize(scalar);
    text(num, v.x, v.y + scalar); //iのj乗をマスに表示
    num = (num * i) % mod;  //numをiの(j+1)乗に更新
  }
}
