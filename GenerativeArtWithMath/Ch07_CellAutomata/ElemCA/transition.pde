//8個の01要素からなる配列ruleに対して，遷移ルールを決定する
int transition(int a, int b, int c){
  int d;
  //abcを10進数に置き換える
  int ruleInt = int(a * pow(2, 2) + b * pow(2, 1) + c * pow(2, 0));
  d = rule[7 - ruleInt];
  return d;
}
