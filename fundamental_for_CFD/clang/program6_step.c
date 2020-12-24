/************************************************
 step.cpp
 急拡大流路の流れ
 （SMAC法）
************************************************/

// fopen を fopen_s にすることを強要させない
#define _CRT_SECURE_NO_WARNINGS

//===========================
// 変数の定義など
//===========================
//***** cavity_fvm_smac.cpp と cavity_simple.cpp で共通するもの *****
#include <stdio.h>
#include <math.h>
#define MAX(a,b) ((a)>(b)?(a):(b))
#define MIN(a,b) ((a)<(b)?(a):(b))

#define ni 50       // 格子分割数（x方向）
#define nj 10       // 格子分割数（y方向）

double uin= 2,      // 流入速度
       Lx= 5, Ly= 1,      // 計算領域の大きさ（x方向，y方向）
       re= 100,           // レイノルズ数
       //re= 20,
       alpha= 1.8,        // SORの加速係数
       converge= 1.e-6,   // SORの収束の判定基準
       u[ni+2][nj+2] , v[ni+2][nj+2],   // 速度
       p[ni+2][nj+2], pc[ni+2][nj+2],   // 圧力，圧力補正
       ae[ni+2][nj+2], aw[ni+2][nj+2],  // 離散化方程式の係数
       an[ni+2][nj+2], as[ni+2][nj+2],
       ap[ni+2][nj+2], b[ni+2][nj+2],
       dx, dy,    // 格子サイズ
       visc,      // 動粘性係数
       dvmax,     // 速度vの変化量の最大値
       pcnew, err;
int i, j, iter ;

void sor_pc();  // 圧力補正のポアソン式をSOR法で解くルーチン
void PlotVector(), Plot_Presure_Streamline(); // 結果の表示ルーチン
FILE *fp  = _popen("C:/gnuplot/bin/gnuplot", "w");;   // gnuplotとの接続のためのファイルポインタ
FILE *fp1 = _popen("C:/gnuplot/bin/gnuplot", "w");

//**** それ以外のもの *****

//double cn= 0.5,       // 時間きざみを決定する係数 (0<cn<1) Re=100, 50x10, 中心法　-> 発散
//double cn= 0.3,       // 時間きざみを決定する係数 (0<cn<1) Re=100, 50x10, 中心法　-> OK
//double cn= 0.4,       // 時間きざみを決定する係数 (0<cn<1) Re=100, 50x10, 中心法　-> OK
double cn= 0.4,         // 時間きざみを決定する係数 (0<cn<1)

       unew[ni+2][nj+2], vnew[ni+2][nj+2],  // 新しい時刻の速度
       flux_e[ni+2][nj+2],  // 検査体積の右界面の流束
       flux_n[ni+2][nj+2],  // 検査体積の上界面の流束
       ue,vn,               // セル界面の速度（右界面，上界面）
       time, dt,      // 時刻、時間きざみ
       dudt, dvdt, vold;

int n,          // 時刻ステップ
    nend= 2000;     // 計算ステップ数

//===========================
// メインプログラム
//===========================

int main() {
  //***** gnuplot と接続 *****
  fprintf(fp , "set terminal wxt title '速度ベクトル' size 1000,300 \n");
  fprintf(fp1, "set terminal wxt title '圧力と流線' size 1000,300 \n");

  //***** 定数 *****
  visc = 1./re;
  dx = Lx/(double)ni ; dy = Ly/(double)nj ;
  dt = cn/(uin/dx + 2*visc*(1/dx/dx + 1/dy/dy));

  //***** 圧力ポアソン方程式の係数 *****
  for(i=1; i<=ni; i++){
  for(j=1; j<=nj; j++){
    aw[i][j] = 1./dx/dx ; ae[i][j] = 1./dx/dx ;
    as[i][j] = 1./dy/dy ; an[i][j] = 1./dy/dy ;
    ap[i][j] = 2./dx/dx + 2./dy/dy;

  // 境界条件を考慮した係数の変更
    if(j==1) {ap[i][j] = ap[i][j] - as[i][j] ; as[i][j] = 0;}
    if(j==nj){ap[i][j] = ap[i][j] - an[i][j] ; an[i][j] = 0;}
    if(i==1) {ap[i][j] = ap[i][j] - aw[i][j] ; aw[i][j] = 0;}
    if(i==ni){ap[i][j] = ap[i][j] + ae[i][j] ; ae[i][j] = 0;}
  }}

  // 流入境界における速度
  i=0;
  for(j=nj/2+1; j<=nj; j++) {
    u[i][j] = uin;
    unew[i][j] = uin;
  }

  //**************************************************
  // 時間前進
  //**************************************************
do {
  n = n+1;
  time = time + dt;

  //***** 境界条件 *****
  for(i=1; i<=ni; i++){
    u[i][0] = -u[i][1];     // 下境界 u=0
    u[i][nj+1] = -u[i][nj]; // 上境界 u=0
  }
  for(j=1; j<=nj; j++){
    v[0][j] = -v[1][j];     // 左境界 v=0
    v[ni+1][j] = v[ni][j];  // 右境界 dv/dx = 0
    u[ni+1][j] = u[ni][j];  // 右境界 du/dx = 0
    p[ni+1][j] = -p[ni][j]; // 右境界 p = 0
  }

  //***** NS方程式：次の時刻の速度の仮の値 *****

  //----- unew -----
  // 流束
  for(i=0; i<=ni; i++){
  for(j=0; j<=nj; j++){
    ue = (u[i][j]+u[i+1][j])/2;
    vn = (v[i][j]+v[i+1][j])/2;

//    flux_e[i][j] = ue*(u[i+1][j]+u[i][j])/2 - (visc/dx + fabs(ue)/2)*(u[i+1][j]-u[i][j]); //１次風上
//    flux_n[i][j] = vn*(u[i][j+1]+u[i][j])/2 - (visc/dy + fabs(vn)/2)*(u[i][j+1]-u[i][j]); //１次風上

    flux_e[i][j] = ue*(u[i+1][j]+u[i][j])/2 - visc/dx*(u[i+1][j]-u[i][j]);              //中心
    flux_n[i][j] = vn*(u[i][j+1]+u[i][j])/2 - visc/dy*(u[i][j+1]-u[i][j]);              //中心
  }}

  // 次の時刻の値
  for(i=1; i<=ni; i++){
  for(j=1; j<=nj; j++){
    dudt = (flux_e[i-1][j]-flux_e[i][j])/dx + (flux_n[i][j-1]-flux_n[i][j])/dy + (p[i][j]-p[i+1][j])/dx;
    unew[i][j] = u[i][j]+dudt*dt;
  }}

  //----- vnew -----
  // 流束
  for(i=0; i<=ni; i++){
  for(j=0; j<=nj-1; j++){
    ue = (u[i][j]+u[i][j+1])/2;
    vn = (v[i][j]+v[i][j+1])/2;

//    flux_e[i][j] = ue*(v[i+1][j]+v[i][j])/2 - (visc/dx + fabs(ue)/2)*(v[i+1][j]-v[i][j]); //１次風上
//    flux_n[i][j] = vn*(v[i][j+1]+v[i][j])/2 - (visc/dy + fabs(vn)/2)*(v[i][j+1]-v[i][j]); //１次風上

    flux_e[i][j] = ue*(v[i+1][j]+v[i][j])/2 - visc/dx*(v[i+1][j]-v[i][j]);              //中心
    flux_n[i][j] = vn*(v[i][j+1]+v[i][j])/2 - visc/dy*(v[i][j+1]-v[i][j]);              //中心
  }}

  // 次の時刻の値
  for(i=1; i<=ni; i++){
  for(j=1; j<=nj-1; j++){
    dvdt = (flux_e[i-1][j]-flux_e[i][j])/dx + (flux_n[i][j-1]-flux_n[i][j])/dy + (p[i][j]-p[i][j+1])/dy;
    vnew[i][j] = v[i][j]+dvdt*dt;
  }}

  //***** 速度のdivergence *****
  for(i=1; i<=ni; i++){
  for(j=1; j<=nj; j++){
    b[i][j] = -( (unew[i][j]-unew[i-1][j])/dx + (vnew[i][j]-vnew[i][j-1])/dy )/dt;
    pc[i][j] = 0;
  }}

  //***** 圧力補正pcをSOR法で解く *****
  sor_pc();

  //***** 速度・圧力を修正し，前の値を更新する *****
  i=ni;
  for(j=1; j<=nj; j++) pc[i+1][j] = -pc[i][j];

  dvmax = 0.;
  for(i=1; i<=ni; i++){
  for(j=1; j<=nj; j++){
    vold = v[i][j];
    u[i][j] = unew[i][j] + (pc[i][j]-pc[i+1][j])/dx*dt;
    if(j != nj) v[i][j] = vnew[i][j] + (pc[i][j]-pc[i][j+1])/dy*dt;
    p[i][j] = p[i][j] + pc[i][j];
    dvmax = MAX(dvmax, fabs(v[i][j]-vold));
  }}

 //***** 途中結果の表示 *****
  printf("n=%d iter=%d dvmax=%f\n",n,iter,dvmax);

  if(n % 200 ==0){  // 計算途中の速度ベクトル図を描く（計算の進行状況を見たいとき）
    PlotVector();
    //Plot_Presure_Streamline();
  }

  if(n==1){
    PlotVector();
    //Plot_Presure_Streamline();
    printf("一時停止中。\n テキスト画面，グラフィック画面の位置や大きさを調節し，Enter キーを押して下さい。");
    getchar();
  }
}while (n<nend);

  //***** 計算終了後の表示 *****
  PlotVector();              // 速度ベクトル
  Plot_Presure_Streamline(); // 圧力分布と流線
  printf("計算終了。\n");
  getchar();
}

//**********************************************************************
// 圧力補正のポアソン方程式をSOR法で解く
//**********************************************************************
void sor_pc(){
  iter = 0;
  do {
    iter = iter + 1;
    err = 0. ;
    for(i=1; i<=ni; i++) {
    for(j=1; j<=nj; j++){
      pcnew = (aw[i][j]*pc[i-1][j]+ae[i][j]*pc[i+1][j]
              +as[i][j]*pc[i][j-1]+an[i][j]*pc[i][j+1]+b[i][j])/ap[i][j] ;
      err = MAX(err, fabs(pcnew-pc[i][j]));
      pc[i][j] = pc[i][j] + alpha*(pcnew-pc[i][j]);
    }}
    if(iter%10000 == 0) {
      printf("iter=%d err=%f \n",iter, err);
      //getchar();
    }
  } while (err>converge);
}

//**********************************************************************
// 結果のグラフィック表示（gnuplot）
//**********************************************************************

//======================================================================
// 速度ベクトル図（gnuplot）
//======================================================================
void PlotVector(){
  double uc, vc;
  int mabiki=1;

  fprintf(fp, "set xrange [0:5]\n");
  fprintf(fp, "set yrange [0:1]\n");
  fprintf(fp, "set size ratio 0.2 \n");
  fprintf(fp, "unset key \n");

  if (ni >= 25) mabiki = ni/25;

  fprintf(fp, "plot '-' with vectors lc rgb 'black' \n");
  for(i=1; i<=ni; i+=mabiki) {
    for(j=1; j<=nj; j++){
      uc = (u[i][j]+u[i-1][j])/2; //セル中心の速度
      vc = (v[i][j]+v[i][j-1])/2;
      fprintf(fp, "%f %f %f %f\n", (i-0.5)*dx, (j-0.5)*dy, uc/uin*dx*0.95*mabiki, vc/uin*dx*0.95*mabiki);
    }}
  fprintf(fp, "e \n");
  fflush(fp);
}

//======================================================================
// 圧力分布の色塗り図と流線の重ね描き（gnuplot）
//======================================================================

void Plot_Presure_Streamline(){
  double x, y, dummy, dd;
  double s[ni+2][nj+2];   // 流れ関数
  int num_cnt;

  //******* 圧力分布 *********
  for(i=1;i<=ni;i++){
    p[i][0] = p[i][1];    // 下境界の圧力
    p[i][nj+1] = p[i][nj];  // 上境界の圧力
  }
  for(j=0;j<=nj+1;j++){
    p[0][j] = p[1][j];    // 左境界の圧力
    p[ni+1][j] = -p[ni][j]; // 右境界の圧力
  }

  //描画の設定
  fprintf(fp1, "set multiplot \n");

  fprintf(fp1, "set xrange [0:%f]\n", Lx);
  fprintf(fp1, "set yrange [0:%f]\n", Ly);
  fprintf(fp1, "set size ratio %f \n", Ly/Lx);

  fprintf(fp1, "set palette defined ( 0 '#0000ff',1 '#00b8ff',2 '#00ffff',3 '#00ffb8',4 '#00ff00',5 '#b8ff00',6 '#ffff00',7 '#ffb800',8 '#ff0000') \n"); // レインボーカラー
  fprintf(fp1, "set pm3d map interpolate %d,%d \n", MAX(100/ni,1), MAX(100/nj,1));   // map -> 3D を上から見た図のみ（サーフェスプロット）。set size ratio が有効になる。

  //fprintf(fp1, "set cbrange[%f:%f] \n", -0.6, 0.3);  // カラーバーの範囲を指定

  //その他設定
  fprintf(fp1, "set surface \n");
  fprintf(fp1, "unset contour \n");
  fprintf(fp1, "set tics \n");

  //描画
  fprintf(fp1, "splot '-' \n");
  for(i=0; i<=ni+1; i++) {
    for(j=0; j<=nj+1; j++){
      x=(i-0.5)*dx ; x=MAX(x,0.) ; x=MIN(x,Lx);
      y=(j-0.5)*dy ; y=MAX(y,0.) ; y=MIN(y,Ly);
      if(i==ni+1) dummy = 0;
      else dummy = p[i][j];
      fprintf(fp1, "%f %f %f\n", x, y, dummy);
    }
    fprintf(fp1, "\n");
  }
  fprintf(fp1, "e \n");
  fflush(fp1);

   //******* 流線 *********
  //流れ関数
  for(i=0; i<=ni; i++){
    s[i][0] = 0;
    for(j=1; j<=nj; j++){
      s[i][j] = s[i][j-1] + u[i][j]*dy;
    }
  }

  //描画の設定
  fprintf(fp1, "set contour \n");
  num_cnt=10;
  dummy = uin*Ly/2;
  dd = dummy/num_cnt;
  fprintf(fp1, "set cntrparam levels incremental %f,%f,%f \n", -dummy, dd,dummy); //等高線の下限，間隔，上限
  //fprintf(fp1, "set cntrparam levels %d \n", num_cnt);

  //等高線の色を単色
  fprintf(fp1, "set style line 1 lt 1 lw 1 lc rgb '#000000' \n");
  fprintf(fp1, "set style increment user \n");
  fprintf(fp1, "unset clabel \n");

  //その他設定
  fprintf(fp1, "unset key \n");
  fprintf(fp1, "unset tics \n");
  fprintf(fp1, "set nosurface \n");

  //描画
  fprintf(fp1, "splot '-' with lines\n");
  for(i=0; i<=ni; i++) {
    for(j=0; j<=nj; j++){
      fprintf(fp1, "%f %f %f\n", i*dx, j*dy, s[i][j]);
      }
    fprintf(fp1, "\n");
  }
  fprintf(fp1, "e \n");
  fflush(fp1);

  fprintf(fp1, "unset multiplot \n");
}
