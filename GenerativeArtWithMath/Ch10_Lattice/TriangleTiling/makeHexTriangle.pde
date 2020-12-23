void makeHexTriangle(){
  tile = createShape(GROUP);  //PShapeのグループを作る
  for (int i = 0; i < 6; i++){
    PVector v = PVector.fromAngle(PI * i / 3 + PI / 6);
    v.mult(scalar / pow(sqrt(3), 2));
    PShape tri = makeTriangle();  //三角形を作る
    tri.translate(v.x, v.y);
    tri.rotate(PI * i);
    tile.addChild(tri); //三角形をグループに加える
  }
}
