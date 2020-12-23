void rhombDivision(){
  ArrayList<Tri> nextT = new ArrayList<Tri>(); //次の三角形の集合
  ArrayList<Tri> nextF = new ArrayList<Tri>();
  translate(width / 2, height / 2);
  fill(col[0]); //細いひし形の色
  for (Tri t : listT){  //細い三角形の描画と生成
    t.drawRhomb();
    t.divThinS(nextT, nextF);
  }
  fill(col[1]); //太いひし形の色
  for (Tri t : listF){ //太い三角形の描画と生成
    t.drawRhomb();
    t.divFatL(nextT, nextF);
  }
  listT = nextT;
  listF = nextF;
}
