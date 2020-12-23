void initialize(){
  for (int i = 0; i < num; i++){
    for (int j = 0; j < num; j++){
      if (i == num / 2 && j == num / 2){
        state[i][j] = 1;  // 真ん中の成分のみ1
      } else {
        state[i][j] = 0;
      }
    }
  }
}
