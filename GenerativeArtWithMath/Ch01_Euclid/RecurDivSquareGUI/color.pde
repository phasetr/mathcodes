void setColor(){
  if (rand.length <= count){
    rand = append(rand, random(1));
  }
  fill(color(rand[count], 1, 1));
  count++;
}
void changeColor(){
  for (int i = 0; i < rand.length; i++){
    rand[i] = random(1);
  }
}
