void controller(){
  cp5 = new ControlP5(this);
  cp5.addSlider("gap")
       .setPosition(10,10)
       .setSize(100,20)
       .setRange(0, 1)
       .setValue(0.5)
       .setColorCaptionLabel(0)
       ;
}
