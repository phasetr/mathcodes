import controlP5.*;
import processing.pdf.*;
ControlP5 cp5;
float ratio, thr, thr2;
float[] rand1, rand2;  //ランダムな数値を格納する配列
boolean mond = false;
boolean rec = false;  //画像保存のための論理型変数
int count;  //描画した長方形の個数を記録する変数
String namePDF, namePNG;
void setup(){
  size(500, 500);
  colorMode(HSB, 1);
  controller();  //controller関数を呼び出し
  rand1 = new float[0];  //0個の配列を生成
  rand2 = new float[0];
}
void draw(){
  background(1, 0, 1);
  ratio = (1 + sqrt(5)) / 2;
  count = 0;
  if (rec){
    String timeStamp = str(year()) + str(month()) + str(day()) + str(hour()) + str(minute()) + str(second());
    namePDF = str(int(thr)) + "_" + timeStamp + ".pdf";  //PDFの保存ファイル名
    namePNG = str(int(thr)) + "_" + timeStamp + ".png";  //PNGの保存ファイル名
    beginRecord(PDF, namePDF);
  }
  if(ratio != 1){
    colRect(0, 0, width, width);
    divSq(0, 0, width);
  }
  if(rec){
    endRecord();
    save(namePNG);
    rec = false;
  }
}
