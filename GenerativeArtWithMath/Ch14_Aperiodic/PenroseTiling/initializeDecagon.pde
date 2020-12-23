void initializeDecagon(float scalar){
  for (int i= 0; i < 10; i++){  //初期配置
    PVector v0 = new PVector(0, 0, 0);  //原点
    PVector v1 = PVector.fromAngle(i * 2 * PI / 10);
    v1.mult(scalar);
    PVector v2 = PVector.fromAngle((i+1) * 2 * PI / 10);
    v2.mult(scalar);
    if (i % 2 == 0){
      listT.add(new Tri(v0, v1, v2));
    } else {
      listT.add(new Tri(v0, v2, v1));
    }
  }
}
