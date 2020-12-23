void colRect(float x, float y, float z, float w){
  if (rand1.length <= count){
    rand1 = append(rand1, random(1)); //ランダムに選んだ数を新しい要素として配列hに加える
    rand2 = append(rand2, random(1)); //ランダムに選んだ数を新しい要素として配列hに加える
  }
  if (mond){
    mondCol(rand2[count]);
  } else {
    fill(color(rand2[count], 1, 1));
    strokeWeight(1);
  }

  rect(x, y, z, w);
  count++;
}
void changeCol(){
  for (int i = 0; i < rand1.length; i++){
    rand1[i] = random(1);  //ランダムに数を選んで配列の要素を書き換える
    rand2[i] = random(1);
  }
}
void mondCol(float val){
  color col;
  if (val < 0.15){
    col = color(0, 1, 1); //赤
  }else if (val < 0.3){
    col = color(2.0 / 3, 1, 1); //青
  }else if (val < 0.45){
    col = color(1.0 / 6, 1, 1); //黄
  }else if (val < 0.5){
    col = color(0, 1, 0); //黒
  } else if (val < 0.7){
    col = color(0, 0, 0.9); //灰
  } else {
    col = color(0, 0, 1); //白
  }
  fill(col);
  strokeWeight(5);
}
