PVector[][] lattice;
PShape tile;
PVector[] base = new PVector[2];
int num = 10;
float scalar;
int upperLimit = 0;
void setup(){
  size(500, 500, P2D);
  colorMode(HSB, 1);
  background(1, 0, 1);
  scalar = height * 1.0 / num;
  makeHexVector();
  makeLattice();
  deformHex();
  drawTiling();
}
void draw(){}
void mouseClicked(){
  background(1, 0, 1);
  upperLimit++;
  deformHex();
  drawTiling();
}
