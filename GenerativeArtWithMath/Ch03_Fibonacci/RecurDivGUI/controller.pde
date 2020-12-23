void controller(){
  cp5 = new ControlP5(this);
  cp5.addSlider("num")
  .setPosition(10,10)
  .setSize(100,20)
  .setRange(1,20)
  .setValue(10)
  .setColorCaptionLabel(0)
  ;
  cp5.addSlider("thr")
  .setPosition(10,50)
  .setSize(100,20)
  .setRange(0,9)
  .setValue(0)
  .setColorCaptionLabel(0)
  ;
  cp5.addBang("rec")
  .setPosition(10, 90)
  .setSize(50, 20)
  .setColorCaptionLabel(0)
  ;
}
