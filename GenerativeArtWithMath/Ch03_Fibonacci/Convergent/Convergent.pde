int m = 1;
int num = 10; //数列の項数
float x = m;
float alpha = (m + sqrt(m * m + 4)) / 2;  //収束先の循環連分数
size(500, 200);
float limPos = map(alpha, m, m + 1, 0, height); //収束先の位置
stroke(255, 0, 0);  //漸近線の色(赤)
line(0, limPos, width, limPos); //漸近線
float step = (float) width / num; //数列の項が増加するごとのx位置の増分
stroke(0); //数列のグラフの色(黒)
//数列を順に計算し，線分でつなぐ
for(int i = 0; i < num; i++){
  float nextX = m + 1.0 / x; //漸化式
  float pos = map(x, m, m + 1, 0, height);  //i項目の数の位置
  float nextPos = map(nextX, m, m + 1, 0, height);  //i+1項目の数の位置
  line(i * step, pos, (i + 1) * step, nextPos);  //線分の描画
  x = nextX;  //次の項を計算するために数を更新
}
