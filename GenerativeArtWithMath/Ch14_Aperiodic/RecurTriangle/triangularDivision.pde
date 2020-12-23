void triangularDivision(){
  ArrayList<Tri> nextT = new ArrayList<Tri>(); //次の細い三角形のリスト
  ArrayList<Tri> nextF = new ArrayList<Tri>();  //次の太い三角形のリスト
  translate(width / 2, height / 2);
  fill(col[0]); //細い三角形の色(initialize関数でランダムに設定)
  for (Tri t : listT){  //グローバル変数listTは細い三角形のリスト
    t.drawTriangle(); //細い三角形の描画
    t.divThinS(nextT, nextF); //Thin<Fatとなる分割
    // t.divThinL(nextT, nextF);  //Fat<Thinとなる分割
  }
  fill(col[1]); //太い三角形の色(initialize関数でランダムに設定)
  for (Tri t : listF){  //グローバル変数listFは太い三角形のリスト
    t.drawTriangle(); //太い三角形の描画
    t.divFatL(nextT, nextF);  //Thin<Fatとなる分割
    // t.divFatS(nextT, nextF); //Fat<Thinとなる分割
  }
  listT = nextT;  //リストの更新
  listF = nextF;
}
