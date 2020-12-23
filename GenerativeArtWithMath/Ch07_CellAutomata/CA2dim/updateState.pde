void updateState(){
  int[][] nextState = new int[num][num]; // 次世代の状態
  for (int i = 0; i < num; i++){
    for (int j = 0; j < num; j++){
      nextState[i][j] = transition(i, j); // 遷移
    }
  }
  state = nextState;  //更新
}
