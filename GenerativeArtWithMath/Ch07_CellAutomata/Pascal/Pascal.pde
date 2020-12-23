int num = 8; //計算する世代数の上限
int[] state = {1};  //初期状態
int gen = 0;  //世代
void setup(){
  size(500, 500);
}
void draw(){
  if(gen < num){
    drawNumber(gen);  //数字を書く
    updateState();  //状態を更新する
  }
}
