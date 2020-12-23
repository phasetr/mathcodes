int num = 250;  //表示する世代数
int mod = 2;  //法とする数
int[] state = {1};  //初期状態
int gen = 0;
void setup(){
  size(1000, 500);
  colorMode(HSB, 1);
  background(0, 0, 1);
}
void draw(){
  if (gen < num){
    drawCell(gen);
    updateState();
  }
}
void mouseClicked(){
  gen = 0;
  state = new int[]{1};  //初期状態
  mod = int(random(2, 20));
  println(mod);
  background(0, 0, 1);
}
