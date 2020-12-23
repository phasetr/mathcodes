void colRect(float xPos, float yPos, float wd, float ht, int ind){
  float scalar = (float) width / fibo[0];
  fill((ind * 1.0 / num) % 1, 1, 1);
  rect(scalar * xPos, scalar * yPos, scalar * wd, scalar * ht);
}
