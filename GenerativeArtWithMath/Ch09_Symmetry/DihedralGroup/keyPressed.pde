void keyPressed() {
  if (key == 's'){  //鏡映
    j *= -1;
    println(key);
  } else if (key == 'r'){  //回転
    k = (k + j + gon) % gon;
    println(key);
  } else if (key == 'e'){ //初期化
    k = 0;
    j = 1;
    println("RESET");
  }
  drawShape();
}
void draw(){}
