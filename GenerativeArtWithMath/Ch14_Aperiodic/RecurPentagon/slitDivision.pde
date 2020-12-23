void slitDivision(){  //描画と分割
  ArrayList<Pent> nextPent = new ArrayList<Pent>(); //次のリスト
  background(0, 0, 1);
  translate(width / 2, height / 2);
  fill(col);
  for (Pent p : listPent){
    p.drawPent(); //正五角形を描画
    p.divPent(nextPent);  //新たな正五角形をリストに追加
  }
  listPent = nextPent;  //リストを更新
}
