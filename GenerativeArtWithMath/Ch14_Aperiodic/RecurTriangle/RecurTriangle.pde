ArrayList<Tri> listT = new ArrayList<Tri>();  //細い三角形のリスト
ArrayList<Tri> listF = new ArrayList<Tri>(); //太い三角形のリスト
color[] col;  //三角形の色のための変数
void setup(){
  size(500, 500);
  colorMode(HSB, 1);
  initialize(1200);
  // initializeDecagon(1200);
  triangularDivision();
}
void draw(){}
void mouseClicked(){
  triangularDivision();
}
