int num = 250;  //表示する世代数
int mod = 2;  //法とする数
int[] state = {1};  //初期状態
int[]  rule = {0, 0, 0, 1, 1, 1, 1, 0};
float gen = 0;
void setup(){
  size(1000, 500);
  colorMode(HSB, 1);
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
  rule = new int[8];
  int ruleInt = 0;
  for (int i = 0; i < 8; i++){
    rule[i] = int(random(2));
    ruleInt += rule[i] * int(pow(2, 7 - i));
  }
  println(ruleInt);
  background(0, 0, 1);
}
