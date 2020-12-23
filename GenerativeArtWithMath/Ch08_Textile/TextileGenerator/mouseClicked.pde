void mouseClicked(){
  int x = floor(mouseX / scalar);
  int y = floor(mouseY / scalar);
  if (y < columnA){  //マウスのカーソルがBまたはCの上にある場合
    if (x < columnA){   //マウスのカーソルがBの上にある場合
      mtxB[y][x] = (mtxB[y][x] + 1) % 2;  //行列の成分が0ならば1，1ならば0に変える
    } else {   //マウスのカーソルがCの上にある場合
      mtxC[y][x - columnA] = (mtxC[y][x - columnA] + 1) % 2;
    }
  } else if (x < columnA) {  //マウスのカーソルがAの上にある場合
      mtxA[y - columnA][x] = (mtxA[y - columnA][x] + 1) % 2;
  }
}
