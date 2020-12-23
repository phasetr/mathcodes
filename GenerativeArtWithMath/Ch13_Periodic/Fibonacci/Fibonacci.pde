String word = "A";  //初期状態
int gen = 0;  //世代数
void setup(){
  transition(); //遷移
}
void draw(){}
void transition(){
  int numA = 0; //Aの個数
  int numB = 0; //Bの個数
  println(gen, ":" + word); //世代数とその状態を表示
  String[] splitW = splitTokens(word); //ワードを1文字ずつの配列に変換
  for(int i = 0; i < splitW.length; i++){
    if(splitW[i].equals("A")){
      splitW[i] = "A B"; //A->AB
      numA++;   //Aの個数を増やす
    } else {
      splitW[i] = "A"; //B->A
      numB++;   //Bの個数を増やす
    }
  }
  println(numA, numB);  //AとBの個数を表示
  word = join(splitW, " ");  //配列をスペースでつなげる
  gen++;  //世代数を増やす
}
void mouseClicked(){
  transition();
}
