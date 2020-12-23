void updateState(){
  int[] BOUNDARY = {0};
  int[] nextState = new int[state.length + 1]; // 次の世代の状態
  state = splice(state, BOUNDARY, 0);  // 配列の最初に境界値を加える
  state = splice(state, BOUNDARY, state.length); // 配列の最後に境界値を加える
  for (int i = 0; i < state.length - 1; i++){
    nextState[i] = transition(i);  // 次世代の状態の計算
  }
  state = nextState; // 状態を更新
  gen++;  //世代を1つ増やす
}
