PVector[] getVector(PVector[] vec){
  PVector[] nextVec = new PVector[4];
  for(int i = 0; i < 4; i++){
    PVector dir = PVector.sub(vec[(i + 1) % 4], vec[i]);  //2頂点間の方向ベクトル
    dir.mult(gap);  //ずれの分を方向ベクトルにかける
    nextVec[i] = PVector.add(vec[i], dir); //元の頂点の位置ベクトルをずらして新たなベクトルを作る
  }
  return(nextVec);
}
