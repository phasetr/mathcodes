import controlP5.*;  //ControlP5ライブラリを読み込み
import processing.pdf.*;  //ControlP5クラスの変数を宣言
ControlP5 cp5;
int numA, numB;
float ratio, thr;
float[] rand = new float[0];  //ランダムな数値を格納する配列
boolean rec = false;  //画像保存のための論理型変数
int count;  //描画した長方形の個数を記録する変数
void setup(){
  size(500, 500);
  colorMode(HSB, 1);
  controller();  //cp5のコントローラを呼び出し
}
void draw(){
  background(1, 0, 1);
  ratio = (float) numB / numA;
  count = 0;
  if (rec){
    String namePDF = str(numA) + "_" + str(numB) + "_" + str(int(thr)) + ".pdf";  //PDFの保存ファイル名
    beginRecord(PDF, namePDF);
  }
  if(ratio != 1){  //numAとnumBが異なるとき実行
    divSq(0, 0, width);
  }
  if(rec){
    endRecord();
    String namePNG = str(numA) + "_" + str(numB) + "_" + str(int(thr)) + ".png";  //PNGの保存ファイル名
    save(namePNG);
    rec = false;
  }
}
