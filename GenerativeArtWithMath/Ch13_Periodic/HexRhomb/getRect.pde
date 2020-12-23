PShape getRect(PShape rhomb){
  PShape rect = createShape();
  rect.setFill(color(0.7, 1, 1));
  rect.beginShape();
  for (int i = 0; i < 4; i++){
    PVector v = rhomb.getVertex(i);
    v.add(rhomb.getVertex((i + 1) % 4));
    v.mult(0.5);
    rect.vertex(v.x, v.y);
  }
  rect.endShape(CLOSE);
  return rect;
}
