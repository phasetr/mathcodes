import processing.pdf.*;  //PDF保存のためのライブラリを読み込み
int numA = 10;
int numB = 6;
float ratio = (float) numB / numA;  //比率
float wd = width;    //長方形の縦または横の長さ
float xPos = 0;
float yPos = 0;
int itr = 0;
String namePDF = str(numA) + "_" + str(numB) +".pdf";  //PDFの保存ファイル名
String namePNG = str(numA) + "_" + str(numB) +".png";  //PNGの保存ファイル名
size(500, 500);
beginRecord(PDF, namePDF);  //PDF形式データの保存開始
colorMode(HSB, 1);
while (wd > 0.1){   //wdが許容誤差(0.1)より大きければ以下を実行
  itr++;
  if (itr % 2 == 1){
    while (xPos + wd * ratio < width + 0.1){  //xPos+wd*ratioがwidthを超えないとき以下の文を実行
      fill(color(random(1), 1, 1));
      rect(xPos, yPos, wd * ratio, wd);      //(xPos,yPos)を左上の頂点とする縦wd横wd*ratioの長方形を描画
      xPos += wd * ratio;                //xPosにwd*ratioを加える
    }
    wd = width - xPos;             //wdにwidth-xPosを代入
  } else {
    while (yPos + wd / ratio < width + 0.1){  //yPos+wd/ratioがwidthを超えないとき以下の文を実行
      fill(color(random(1), 1, 1));
      rect(xPos, yPos, wd, wd / ratio);      //(xPos,yPos)を左上の頂点とする縦wd/ratio横wdの長方形を描画
      yPos += wd / ratio;                //yPosにwd/ratioを加える
    }
    wd = width - yPos;
  }
}
endRecord();  //保存終了
save(namePNG);  //PNG形式で描画ウィンドウを画像保存
