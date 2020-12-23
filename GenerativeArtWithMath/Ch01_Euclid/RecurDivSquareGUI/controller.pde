//グローバル変数をコントローラで操作する
void controller(){
  cp5 = new ControlP5(this);  //コントローラを生成
  cp5.addSlider("numA")  //numAの値を動かすスライダー
    .setPosition(10,10)  //スライダーの位置
    .setSize(100,20)  //スライダーのサイズ
    .setRange(1,40)  //最小値と最大値
    .setValue(10)  //初期値
    .setColorCaptionLabel(0)  //スライダーの文字の色
    ;
  cp5.addSlider("numB")
    .setPosition(10,50)
    .setSize(100,20)
    .setRange(1,40)
    .setValue(6)
    .setColorCaptionLabel(0)
    ;
  cp5.addSlider("thr")
    .setPosition(10,90)
    .setSize(100,20)
    .setRange(10,300)
    .setNumberOfTickMarks(30)
    .setValue(100)
    .setColorCaptionLabel(0)
    ;
  cp5.addBang("changeColor")
    .setPosition(10, 130)
    .setSize(50, 20)
    .setColorCaptionLabel(0)
    ;
  cp5.addBang("rec")
    .setPosition(10, 170)
    .setSize(50, 20)
    .setColorCaptionLabel(0)
    ;
}
