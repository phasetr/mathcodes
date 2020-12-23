void kiteDartDivision(){
  ArrayList<Tri> nextT = new ArrayList<Tri>(); //次の三角形の集合
  ArrayList<Tri> nextF = new ArrayList<Tri>();
  translate(width / 2, height / 2);
  fill(col[0]);   //カイトの色
  for (Tri t : listT){  //細い三角形の描画と生成
    t.drawKiteDart();
    t.divThinL(nextT, nextF);
  }
  fill(col[1]); //ダートの色
  for (Tri t : listF){ //太い三角形の描画と生成
    t.drawKiteDart();
    t.divFatS(nextT, nextF);
  }
  listT = nextT;
  listF = nextF;
}
