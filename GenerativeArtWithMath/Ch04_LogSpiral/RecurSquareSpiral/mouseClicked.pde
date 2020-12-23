void mouseClicked(){
  background(255);
  gap = random(1) / 2;
  drawLogSpiral();
  vec[0] = new PVector(0, 0);
  vec[1] = new PVector(width, 0);
  vec[2] = new PVector(width, height);
  vec[3] = new PVector(0, height);
  println("gap =", gap);
}
