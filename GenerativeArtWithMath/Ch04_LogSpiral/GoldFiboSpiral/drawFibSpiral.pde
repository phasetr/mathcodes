void drawFiboSpiral(){
  float xPos = 0;
  float yPos = 0;
  float scalar = (float) width / (2 * fibo[fibo.length-1]);  //拡大・縮小比率
  for(int i = 1; i < fibo.length - 1; i++){
    rect(scalar * xPos,
      scalar * yPos,
      scalar * SGN[(i+1) % 4] * fibo[i],
      scalar * SGN[i % 4] * fibo[i]);
    arc(scalar * (xPos + SGN[(i+1) % 4] * fibo[i]),
      scalar * (yPos + SGN[i % 4] * fibo[i]),
      scalar * 2 * fibo[i],
      scalar * 2 * fibo[i],
      (1 + i) * PI / 2,
      (2 + i) * PI / 2);
    if (i % 2 == 1){
      xPos += SGN[i % 4] * (fibo[i] + fibo[i+1]);
    } else {
      yPos += SGN[i % 4] * (fibo[i] + fibo[i+1]);
    }
  }
}
