ArrayList<Pent> listPent = new ArrayList<Pent>(); //正五角形のリスト
color col;
void setup(){
  size(500, 500);
  colorMode(HSB, 1);
  initialize(250);  //初期配置
  slitDivision(); //(隙間のある)分割
}
