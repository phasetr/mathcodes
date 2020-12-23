void controller(){
  cp5 = new ControlP5(this);
  cp5.addSlider("hor")
       .setPosition(10,10)
       .setSize(100,20)
       .setRange(-1, (sqrt(3) - 1)/2)
       .setValue(0)
       .setColorCaptionLabel(0)
       ;
  cp5.addSlider("ver")
       .setPosition(10,50)
       .setSize(100,20)
       .setRange(0,1)
       .setValue(0)
       .setColorCaptionLabel(0)
       ;
}
