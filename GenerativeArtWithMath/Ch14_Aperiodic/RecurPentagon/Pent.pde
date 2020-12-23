class Pent{
  PVector[] v_;
  float PHI = (1 + sqrt(5)) / 2;
  Pent(PVector v0, PVector v1){
    v_ = new PVector[6];
    v_[0] = v0;  //正五角形の中心
    for (int i = 1; i < 6; i++){
      v_[i] = PVector.sub(v1, v0);
      v_[i].rotate(2 * i * PI / 5);
      v_[i].add(v0); //正五角形の頂点
    }
  }
  void divPent(ArrayList<Pent> nextList){ //正五角形の分割
    PVector w = PVector.sub(v_[1], v_[0]);
    w.mult(PHI / (2 * PHI + 1));  //次の正五角形の縮小率
    w.rotate(PI / 5);
    w.add(v_[0]);
    nextList.add(new Pent(v_[0], w));  //中央の正五角形をリストに追加
    for (int i = 1; i < 6; i++){
      w = PVector.sub(v_[i], v_[0]);
      w.mult((PHI + 1) / (2 * PHI + 1));
      w.add(v_[0]);
      nextList.add(new Pent(w, v_[i]));  //中央を囲む5つの正五角形をリストに追加
    }
  }
  void drawPent(){  //正五角形の描画
    beginShape();
    for (int i = 1; i < 6; i++){
      vertex(v_[i].x, v_[i].y);
    }
    endShape(CLOSE);
  }
}
