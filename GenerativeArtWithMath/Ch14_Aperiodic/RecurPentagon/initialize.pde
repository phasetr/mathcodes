void initialize(float scalar){
  col = color(random(1), 1, 1);
  PVector v0 = new PVector(0, 0); //正五角形の中心
  PVector v1 = new PVector(scalar, 0); //正五角形の1つの頂点
  listPent.add(new Pent(v0, v1));
}
