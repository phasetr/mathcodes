PVector parameterizeTV08(PVector[] v, int i){
  if(i % 3 == 0){
    v[i].mult(1 + hor);  //垂直方向への頂点移動
  }
  if(i > 1 && i < 5){
    v[i].add(0, -0.5 * ver * scalar/ sqrt(3));  //水平方向への頂点移動
  } else {
    v[i].add(0, 0.5 * ver * scalar/ sqrt(3));  //水平方向への頂点移動
  }
  return v[i];
}
