PVector[] getVector(PVector[] v){  //新しい頂点を生成する
  PVector[] newVec = new PVector[4];
  for(int i = 0; i < 4; i++){
    PVector dir = PVector.sub(v[(i+1)%4], v[i]);  //ずれの方向ベクトル
    dir.mult(gap);  //ずれをa倍する
    newVec[i] = PVector.add(v[i], dir); //元の頂点の位置ベクトルにずれを足して新たなベクトルを作る
  }
  return(newVec);
}
