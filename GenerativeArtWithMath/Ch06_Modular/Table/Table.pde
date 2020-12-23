int mod = 5;  //法とする自然数
size(500, 500);
float scalar = (float) width / mod; //拡大比率
for (int i = 0; i < mod; i++){
  for (int j = 0; j < mod; j++){
    int num = (i + j) % mod;  //数の計算
    PVector v = new PVector(j, i);  //マスの位置
    v.mult(scalar);
    // int num = (i * j) % mod;  //乗法表の場合
    fill(255);  //マスを白くする
    rect(v.x, v.y, scalar, scalar); //マスの描画
    fill(0);  //数字を黒くする
    textSize(scalar);
    text(num, v.x, v.y + scalar);  //数字の表示
  }
}
