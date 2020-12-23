void mouseClicked(){  //マウスクリック時
  sym = true;  //D4対称性を持つ模様の生成
  randomize(mtxB);
}
void keyPressed(){  //キーを押したとき
  sym = false;  //D2対称性を持つ模様の生成
  randomize(mtxB);
}
