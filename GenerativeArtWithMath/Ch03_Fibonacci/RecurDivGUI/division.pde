void divRect(float posX, float posY, int ind, int itr, int sgnX, int sgnY){
  for(int i = 0; i < num-ind; i++){
    float lng = fibo[i+ind];
    int newSgnX = sgnX * SGN[(i+1) % 4];
    int newSgnY = sgnY * SGN[i % 4];
    colRect(posX, posY, newSgnX * lng, newSgnY * lng, ind + i);
    posX += newSgnX * lng;
    posY += newSgnY * lng;
    if (itr < thr){
      divSq(posX, posY, ind+i, itr+1, -newSgnX, -newSgnY);
    }
  }
}
void divSq(float posX, float posY, int ind, int itr, int sgnX, int sgnY){
  for(int i = 0; i < num-ind; i++){
    float lng0 = fibo[i+ind+1];  //小さい方の数
    float lng1 = fibo[i+ind];  //大きい方の数
    int newSgnX = sgnX * SGN[i % 4];
    int newSgnY = sgnY * SGN[(i+1) % 4];
    colRect(posX, posY, newSgnX * lng0, newSgnY * lng1, ind + i + 1);
    posX += newSgnX * lng0;
    posY += newSgnY * lng1;
    if (itr < thr){
      divRect(posX, posY, ind+i+1, itr+1, -newSgnX, -newSgnY);
    }
  }
}
