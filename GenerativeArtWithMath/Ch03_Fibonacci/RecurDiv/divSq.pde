//正方形の位置(xPos, yPos)，フィボナッチ数列の項数ind，
//関数の繰り返し回数itr，正方形の描画に関する符号(sgnX,sgnY)を引数とする分割
void divSquare(float xPos, float yPos, int ind, int itr, int sgnX, int sgnY){
  //(num-ind)項目のフィボナッチ数(=fibo[ind])を一辺とする正方形を順に分割
  for(int i = 0; i < num - ind; i++){
    //フィボナッチ数列の順序を逆にしているため，iが大きいほどフィボナッチ長方形は小さい
    float lng0 = fibo[i + ind + 1]; //フィボナッチ長方形の横幅(短辺)
    float lng1 = fibo[i + ind];  //フィボナッチ長方形の縦幅(長辺)
    int newSgnX = sgnX * SGN[i % 4];  //長方形を描画する方向
    int newSgnY = sgnY * SGN[(i + 1) % 4];
    colRect(xPos, yPos, //フィボナッチ長方形の位置
      newSgnX * lng0, newSgnY * lng1, //フィボナッチ長方形の大きさ
      ind + i + 1); //項数に対応して長方形の色を決定
    xPos += newSgnX * lng0;
    yPos += newSgnY * lng1;
    if (itr < thr){ //関数の繰り返し回数がしきい値未満ならば長方形をフィボナッチ分割
      divRect(xPos, yPos,
        i + ind + 1, //フィボナッチ長方形の短辺の項数を渡す
        itr + 1, //繰り返し回数を1増やして渡す
        -newSgnX, -newSgnY);  //敷き詰めの回り込みの向きを逆にする
    }
  }
}
