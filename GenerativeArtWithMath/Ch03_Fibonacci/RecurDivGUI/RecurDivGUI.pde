import controlP5.*;
import processing.pdf.*;
ControlP5 cp5;
int num, thr;
boolean rec = false;  //画像保存のための論理型変数
int[] fibo;
int[] SGN = {1, 1, -1, -1};
void setup(){
  size(500, 500);
  controller();
}
void draw(){
  generateFibo(num);
  if (rec){
    String namePDF = str(num) + "_" + str(thr) + ".pdf";  //PDFの保存ファイル名
    beginRecord(PDF, namePDF);
  }
  colorMode(HSB, 1);
  background(0, 0, 1);
  divSq(0, 0, 0, 0, 1, 1);
  if(rec){
    endRecord();
    String namePNG = str(num) + "_" + str(thr) + ".png";  //PNGの保存ファイル名
    save(namePNG);
    rec = false;
  }
}
