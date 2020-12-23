void updateState(){
  int[] BOUNDARY = {0};
  int[] nextState = new int[state.length + 1]; // 次の行の配列
  state = splice(state, BOUNDARY, 0);  // 配列stateの最初に{0,0}を加える
  state = splice(state, BOUNDARY, state.length); // 配列stateの最後に{0,0}を加える
  for (int i = 0; i < state.length - 1; i++){
    nextState[i] = transition(i);  // 次世代のセルの状態の計算
  }
  state = nextState; // セルの状態を更新
  gen++;
}
