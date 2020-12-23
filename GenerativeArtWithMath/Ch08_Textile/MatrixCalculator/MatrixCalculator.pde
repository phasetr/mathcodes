int[][] mtxA = {{2, 1}, {0, 1}};  //行列A
int[][] mtxB = {{3}, {1}};  //行列B
void setup(){
  int[][] mtx = multMtx(mtxA, mtxB); //行列AとBの積
  println("mult:");
  for(int i = 0; i < mtx.length; i++){
    println("row:" + i);
    printArray(mtx[i]);  //i行の配列を表示
  }
  mtx = trMtx(mtxA); //行列Aの転置
  println("transpose:");
  for(int i = 0; i < mtx.length; i++){
    println("row:" + i);
    printArray(mtx[i]); //i行の配列を表示
  }
}
