PShape img; //画像用の変数
size(300, 300);
img = loadShape("yosegiC6Part.svg"); //C6対称模様を生成する部分的な模様
// img = loadShape("yosegiD6Part.svg"); //D6対称模様を生成する部分的な模様
// img = loadShape("HelloWorld.svg");
translate(width / 2, height / 2);
for(int j = 0; j < 2; j++){
  for(int k = 0; k < 6; k++){
    img.resetMatrix();
    // img.scale(1, pow(-1, j));  //D6の場合
    img.rotate(k * 2 * PI / 6);
    shape(img);
  }
}
