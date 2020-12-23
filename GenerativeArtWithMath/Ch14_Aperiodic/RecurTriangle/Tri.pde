class Tri{  //三角形のクラス
  float PHI = (1 + sqrt(5)) / 2;  //黄金数
  PVector[] v_;
  Tri(PVector v0, PVector v1, PVector v2){  //コンストラクタ
    v_ = new PVector[]{v0, v1, v2};
  }
  //
  //分割に関するメソッド
  //
  void updateThinS(){  //細い三角形の分割と更新
    PVector v3 = PVector.sub(v_[0], v_[2]);
    v3.mult(2 - PHI);
    v3.add(v_[2]);
    v_ = new PVector[]{v_[1], v_[2], v3};
  }
  void divThinS(ArrayList<Tri> nextThin, ArrayList<Tri> nextFat){  //細い三角形を分割する頂点の生成
    PVector v3 = PVector.sub(v_[0], v_[2]);
    v3.mult(2 - PHI);
    v3.add(v_[2]);
    nextThin.add(new Tri(v_[1], v_[2], v3));
    nextFat.add(new Tri(v3, v_[0], v_[1]));
  }
  void divThinL(ArrayList<Tri> nextThin, ArrayList<Tri> nextFat){
    PVector v3 = PVector.sub(v_[0], v_[2]);
    v3.mult(2 - PHI);
    v3.add(v_[2]);
    PVector v4 = PVector.sub(v_[1], v_[0]);
    v4.mult(1.0 / (PHI + 1));
    v4.add(v_[0]);
    nextThin.add(new Tri(v_[1], v4, v3));
    nextThin.add(new Tri(v_[1], v_[2], v3));
    nextFat.add(new Tri(v4, v3, v_[0]));
  }
  void divFatL(ArrayList<Tri> nextThin, ArrayList<Tri> nextFat){ //太い三角形を分割する頂点の生成
    PVector v3 = PVector.sub(v_[2], v_[1]);
    v3.mult(1.0 / (PHI + 1));
    v3.add(v_[1]);
    PVector v4 = PVector.sub(v_[0], v_[2]);
    v4.mult(1.0 / PHI);
    v4.add(v_[2]);
    nextThin.add(new Tri(v3, v_[0], v4));
    nextFat.add(new Tri(v3, v_[0], v_[1]));
    nextFat.add(new Tri(v4, v_[2], v3));
  }
  void divFatS(ArrayList<Tri> nextThin, ArrayList<Tri> nextFat){
    PVector v3 = PVector.sub(v_[2], v_[1]);
    v3.mult(1 / (PHI + 1));
    v3.add(v_[1]);
    nextThin.add(new Tri(v_[2], v3, v_[0]));
    nextFat.add(new Tri(v3, v_[0], v_[1]));
  }
  //
  //描画に関するメソッド
  //
  void drawTriangle(){ //三角形の描画
    triangle(v_[0].x, v_[0].y, v_[1].x, v_[1].y, v_[2].x, v_[2].y);
  }
  float drawArc(float _radE){  //円弧の描画
    float diam = 2 * PVector.dist(v_[0], v_[2]);
    float radS = _radE - 3 * PI / 5; //円弧の始点のラジアン
    noFill();
    arc(v_[2].x, v_[2].y, diam, diam, radS, _radE);
    return radS;  //円弧の終点の更新
  }
  void drawRhomb(){ //ひし形の描画
    drawShape(v_, new int[]{1, 0, 2});
  }
  void drawKiteDart(){  //カイト&ダートの描画
    drawShape(v_, new int[]{0, 1, 2});
  }
  void drawPentF(color _colP, color _colS){
    PVector v3 = PVector.sub(v_[1], v_[0]);
    v3.mult(PHI - 1);
    v3.add(v_[0]);
    PVector v4 = PVector.sub(v_[2], v_[1]);
    v4.mult(PHI / (PHI + 1));
    v4.add(v_[1]);
    PVector v5 = PVector.sub(v_[0], v_[2]);
    v5.mult(0.5);
    v5.add(v_[2]);
    PVector[] w = {v_[0], v_[1], v_[2], v3, v4, v5};
    noStroke();
    fill(_colP);
    drawShape(w, new int[]{0, 2, 4, 3});
    fill(_colS);
    drawShape(w, new int[]{1, 3, 4});
    noFill();
    stroke(0, 0, 0);
    drawShape(w, new int[]{3, 4, 5});
  }
  void drawPentT(color _colP, color _colS){
    PVector v3 = PVector.sub(v_[1], v_[0]);
    v3.mult(PHI - 1);
    v3.add(v_[0]);
    PVector v4 = PVector.sub(v_[2], v_[1]);
    v4.mult(1 - PHI / 2);
    v4.add(v_[1]);
    PVector v5 = PVector.sub(v_[0], v_[2]);
    v5.mult(0.5);
    v5.add(v_[2]);
    PVector[] w = {v_[0], v_[1], v_[2], v3, v4, v5};
    noStroke();
    fill(_colP);
    drawShape(w, new int[]{0, 2, 4, 3});
    fill(_colS);
    drawShape(w, new int[]{1, 3, 4});
    noFill();
    stroke(0, 0, 0);
    drawShape(w, new int[]{4, 3, 5});
  }
  void drawShape(PVector[] w, int[] ind){
    beginShape();
    for (int i : ind){
      vertex(w[i].x, w[i].y);
    }
    endShape();
  }
}
