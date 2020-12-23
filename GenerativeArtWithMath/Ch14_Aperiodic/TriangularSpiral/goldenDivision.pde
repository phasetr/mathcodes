void goldenDivision(){
  translate(width / 2, height / 2);
  t.drawTriangle(); //三角形の描画
  t.updateThinS();  //三角形を分割し，小さい三角形に更新する
  radE = t.drawArc(radE); //円弧を描き，その角度を更新する
}
