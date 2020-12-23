void colorRect(float xPos, float yPos, float wd, float ht){
  color col;
  float val = random(1);
  if (val < 0.15){  //15%の確率
    col = color(0, 1, 1); //赤
  }else if (val < 0.3){ //15%の確率
    col = color(2.0 / 3, 1, 1); //青
  }else if (val < 0.45){  //15%の確率
    col = color(1.0 / 6, 1, 1); //黄
  }else if (val < 0.5){ //5%の確率
    col = color(0, 1, 0); //黒
  } else if (val < 0.7){  //20%の確率
    col = color(0, 0, 0.9); //灰
  } else {  //30%の確率
    col = color(0, 0, 1); //白
  }
  fill(col);
  strokeWeight(5);  //長方形の枠線の太さ
  rect(xPos, yPos, wd, ht);
}
