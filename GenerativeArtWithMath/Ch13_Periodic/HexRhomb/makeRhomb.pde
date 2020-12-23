void makeRhomb(PVector[] v, color[] col){
  for (int i = 0; i < 6; i += 2){ //1つとばしにiを増やす
    PShape rhomb = createShape();
    rhomb.setFill(col[i / 2]);
    rhomb.beginShape();
    rhomb.vertex(0, 0);
    for (int j = -1; j <= 1; j++){
      rhomb.vertex(v[(i + j + 6) % 6].x, v[(i + j + 6) % 6].y); //正六角形の隣り合う頂点を取る
    }
    rhomb.endShape(CLOSE);
    tile.addChild(rhomb);
    // PShape rect = getRect(rhomb);  //内接する長方形を作る
    // tile.addChild(rect);
  }
}
