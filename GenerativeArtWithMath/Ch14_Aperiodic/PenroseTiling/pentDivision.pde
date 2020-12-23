void pentDivision(){
  background(1, 0, 1);
  translate(width / 2, height / 2);
  ArrayList<Tri> nextT = new ArrayList<Tri>();
  ArrayList<Tri> nextF = new ArrayList<Tri>();
  for (Tri t : listT){
    t.drawPentT(col[0], col[1]);  //col[0]が五角形，col[1]が星の色
    t.divThinS(nextT, nextF);
  }
  for (Tri t : listF){
    t.drawPentF(col[0], col[1]);
    t.divFatL(nextT, nextF);
  }
  listT = nextT;
  listF = nextF;
}
