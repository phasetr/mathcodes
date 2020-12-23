void divRect(float xPos, float yPos, int ind, int itr, int sgnX, int sgnY){
  for(int i = 0; i < num-ind; i++){
    float lng = fibo[i+ind];
    int newSgnX = sgnX * SGN[(i+1) % 4];
    int newSgnY = sgnY * SGN[i % 4];
    colRect(xPos, yPos, newSgnX * lng, newSgnY * lng, ind + i);
    xPos += newSgnX * lng;
    yPos += newSgnY * lng;
    if (itr < thr){
      divSquare(xPos, yPos, ind + i, itr + 1, -newSgnX, -newSgnY);
    }
  }
}
