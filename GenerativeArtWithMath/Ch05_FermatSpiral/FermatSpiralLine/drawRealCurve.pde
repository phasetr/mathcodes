void drawLine(int n){ //中心角の等分線を描く関数
  for(int i = 0; i <= n / 2; i++){
    PVector v = PVector.fromAngle(2 * i * PI / n);  //円周上のn等分点を取る
    v.mult(width / sqrt(2));  //画面いっぱいに線を引くように拡大
    line(v.x, v.y, -v.x, -v.y);
  }
}
void drawRealCurve(float rot){  //連続的なフェルマーらせんを描く関数
  float STEP = 2 * PI * 0.01; //曲線の精度
  float theta = 0;  //偏角
  float rad = 0; //動径
  noFill();
  beginShape(); //頂点をつないで図形を描画
  while(rad <  width / sqrt(2)){
    rad = scalar * sqrt(theta / (2 * PI * rot));
    PVector v = PVector.fromAngle(theta);
    v.mult(rad);
    vertex(v.x, v.y); //頂点をセット
    theta += STEP;
  }
  endShape();
}
