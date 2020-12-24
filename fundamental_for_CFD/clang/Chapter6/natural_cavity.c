/************************************************
 natural_cavity.cpp
 正方形キャビティ内自然対流
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

//#define ni 80       // 格子分割数（x方向）
//#define nj 80       // 格子分割数（y方向）

//#define ni 40       // 格子分割数（x方向）
//#define nj 40       // 格子分割数（y方向）

#define ni 20       // 格子分割数（x方向）
#define nj 20       // 格子分割数（y方向）

double Lx= 1, Ly= 1,      // 計算領域の大きさ（x方向，y方向）
       //ra= 1000000,         // レイリー数
       ra= 10000,         // レイリー数

       pr = 0.71,         // プラントル数
       alpha= 1.8,        // SORの加速係数
       converge= 1.e-6,   // SORの収束の判定基準
       cn= 0.5,           // 時間きざみを決定する係数 (0<cn<1)

       Th = 0.5,
       Tl = -0.5,

       u[ni+2][nj+2] , v[ni+2][nj+2],       // 速度
       unew[ni+2][nj+2], vnew[ni+2][nj+2],  // 新しい時刻の速度
       p[ni+2][nj+2], pc[ni+2][nj+2],       // 圧力，圧力補正
       T[ni+2][nj+2], Tnew[ni+2][nj+2],     // 圧力，圧力補正

       ae[ni+2][nj+2], aw[ni+2][nj+2],      // 離散化方程式の係数 
       an[ni+2][nj+2], as[ni+2][nj+2], 
       ap[ni+2][nj+2], b[ni+2][nj+2],

       flux_e[ni+2][nj+2],  // 検査体積の右界面の流束
       flux_n[ni+2][nj+2],  // 検査体積の上界面の流束

       time, dt,      // 時刻、時間きざみ
       dx, dy,      // 格子サイズ
       gr,          // グラスホッフ数
       visc,        // 動粘性係数
       a,           // 熱拡散率

       ue,vn,       // セル界面の速度（右界面，上界面）
       dudt, dvdt, dTdt,
       pcnew, err,
       smax, smin,

       dvmax,       // 速度vの変化量の最大値
       vmax         // 速度vの最大値
       ;
int n,              // 時刻ステップ
    nend= 5000,     // 計算ステップ数
//    nend= 10000,      // 計算ステップ数
    i, j, iter ;

void sor_pc();  // 圧力補正のポアソン式をSOR法で解くルーチン
void PlotVector(), PlotStreamline(), PlotPresure(), PlotT();  // 結果の表示ルーチン
FILE *fp ;    // gnuplotとの接続のためのファイルポインタ


//===========================
// メインプログラム
//===========================
int main() {
  //***** gnuplot と接続 *****
  fp = _popen("C:/gnuplot/bin/gnuplot", "w"); 
  fprintf(fp, "set terminal wxt title '速度ベクトル' size 600,600 \n");

  //***** 定数 *****
  gr = ra/pr;
  visc = 1./sqrt(gr);
  a = 1./sqrt(gr)/pr;
  dx = Lx/(double)ni ; dy = Ly/(double)nj ;

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
    if(i==ni){ap[i][j] = ap[i][j] - ae[i][j] ; ae[i][j] = 0;}
  }}

  //**************************************************
  // 時間前進
  //**************************************************
do {
  n = n+1;
  dt = cn/(vmax/dy + MAX(visc,a)*2*(1/dx/dx + 1/dy/dy));
  time = time + dt;

  //***** 境界条件 *****
  for(i=1; i<=ni; i++){
    u[i][0] = -u[i][1];     // 下境界 u=0
    u[i][nj+1] = -u[i][nj]; // 上境界 u=0

    T[i][0] = T[i][1];      // 下境界 dT/dy = 0
    T[i][nj+1] = T[i][nj];  // 上境界 dT/dy = 0
  }
  for(j=1; j<=nj; j++){
    v[0][j] = -v[1][j];     // 左境界 v=0
    v[ni+1][j] = -v[ni][j];   // 右境界 v=0

    T[0][j] = 2*Th - T[1][j];     // 左境界 T=Th
    T[ni+1][j] = 2*Tl - T[ni][j];   // 右境界 T = Tl
  }

  //***** NS方程式：次の時刻の速度の仮の値 *****

  //----- unew -----
  // 流束 
  for(i=0; i<=ni-1; i++){
  for(j=0; j<=nj; j++){
    ue = (u[i][j]+u[i+1][j])/2;
    vn = (v[i][j]+v[i+1][j])/2;

//  flux_e[i][j] = ue*(u[i+1][j]+u[i][j])/2 - (visc/dx + fabs(ue)/2)*(u[i+1][j]-u[i][j]); //１次風上
//  flux_n[i][j] = vn*(u[i][j+1]+u[i][j])/2 - (visc/dy + fabs(vn)/2)*(u[i][j+1]-u[i][j]); //１次風上

    flux_e[i][j] = ue*(u[i+1][j]+u[i][j])/2 - visc/dx*(u[i+1][j]-u[i][j]);              //中心
    flux_n[i][j] = vn*(u[i][j+1]+u[i][j])/2 - visc/dy*(u[i][j+1]-u[i][j]);              //中心
  }}
  // 次の時刻の値 
  for(i=1; i<=ni-1; i++){
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

//  flux_e[i][j] = ue*(v[i+1][j]+v[i][j])/2 - (visc/dx + fabs(ue)/2)*(v[i+1][j]-v[i][j]); //１次風上
//  flux_n[i][j] = vn*(v[i][j+1]+v[i][j])/2 - (visc/dy + fabs(vn)/2)*(v[i][j+1]-v[i][j]); //１次風上

    flux_e[i][j] = ue*(v[i+1][j]+v[i][j])/2 - visc/dx*(v[i+1][j]-v[i][j]);              //中心
    flux_n[i][j] = vn*(v[i][j+1]+v[i][j])/2 - visc/dy*(v[i][j+1]-v[i][j]);              //中心
  }}
  // 次の時刻の値 
  for(i=1; i<=ni; i++){
  for(j=1; j<=nj-1; j++){
    dvdt = (flux_e[i-1][j]-flux_e[i][j])/dx + (flux_n[i][j-1]-flux_n[i][j])/dy + (p[i][j]-p[i][j+1])/dy +(T[i][j]+T[i][j+1])/2;  
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

  //***** 速度・圧力を修正する *****
  for(i=1; i<=ni; i++){
  for(j=1; j<=nj; j++){
    if(i != ni) unew[i][j] = unew[i][j] + (pc[i][j]-pc[i+1][j])/dx*dt;
    if(j != nj) vnew[i][j] = vnew[i][j] + (pc[i][j]-pc[i][j+1])/dy*dt;
    p[i][j] = p[i][j] + pc[i][j];
  }}


  //***** 温度場 *****
  // 流束 
  for(i=0; i<=ni; i++){
  for(j=0; j<=nj; j++){
    ue = u[i][j];
    vn = v[i][j];

//  flux_e[i][j] = ue*(T[i+1][j]+T[i][j])/2 - (a/dx + fabs(ue)/2)*(T[i+1][j]-T[i][j]); //１次風上
//  flux_n[i][j] = vn*(T[i][j+1]+T[i][j])/2 - (a/dy + fabs(vn)/2)*(T[i][j+1]-T[i][j]); //１次風上

    flux_e[i][j] = ue*(T[i+1][j]+T[i][j])/2 - a/dx*(T[i+1][j]-T[i][j]);              //中心
    flux_n[i][j] = vn*(T[i][j+1]+T[i][j])/2 - a/dy*(T[i][j+1]-T[i][j]);              //中心
  }}

  // 次の時刻の値 
  for(i=1; i<=ni; i++){
  for(j=1; j<=nj; j++){
    dTdt = (flux_e[i-1][j]-flux_e[i][j])/dx + (flux_n[i][j-1]-flux_n[i][j])/dy;  
    T[i][j] = T[i][j]+dTdt*dt;
  }}

  //***** 速度の更新 *****
  dvmax = 0.;
  vmax = 0;
  for(i=1; i<=ni; i++){
  for(j=1; j<=nj; j++){
    dvmax = MAX(dvmax, fabs(vnew[i][j]-v[i][j]));
    vmax = MAX(vmax, fabs(vnew[i][j]));

    if(i != ni) u[i][j] = unew[i][j];
    if(j != nj) v[i][j] = vnew[i][j];
  }}

  //***** 途中結果の表示 *****
  printf("n=%d iter=%d dvmax=%f\n",n,iter,dvmax); 

  if(n % 200 ==0) PlotVector(); // 計算途中の速度ベクトル図を描く（計算の進行状況を見たいとき）

  if(n==1){
    PlotVector();
    printf("一時停止中。\n テキスト画面，グラフィック画面の位置や大きさを調節し，Enter キーを押して下さい。");
    getchar();
  }

}while (n<nend);

  //***** 計算終了後の表示 *****
  PlotVector();          // 速度ベクトル
  PlotPresure();         // 圧力分布
  PlotStreamline();      // 流線
  PlotT();               // 温度分布
  printf("vmax_Davis = %f \n", vmax*pr*sqrt(gr)); // Davisの無次元速度によるvmax
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
    if(iter%10000 == 0) printf("iter=%d err=%f \n",iter, err);
  } while (err>converge);
}


//**********************************************************************
// 結果のグラフィック表示（gnuplot）
//**********************************************************************

//======================================================================
// 速度ベクトル図（gnuplot）
//======================================================================
void PlotVector(){
  double uc, vc, vscale;
  int mabiki=1;

  if (ni >= 30) mabiki = ni/20;

  fprintf(fp, "set multiplot\n");
  fprintf(fp, "set xrange [0:1]\n");
  fprintf(fp, "set yrange [0:1]\n");
  fprintf(fp, "set size ratio 1 \n");
  fprintf(fp, "unset key \n");

  vscale = vmax;
  if (vmax == 0.) vscale=1;

  fprintf(fp, "plot '-' with vectors lc rgb 'black' \n");
  for(i=1; i<=ni; i++) {
  for(j=mabiki; j<=nj; j+=mabiki){
    uc = (u[i][j]+u[i-1][j])/2; //セル中心の速度
    vc = (v[i][j]+v[i][j-1])/2;
    fprintf(fp, "%f %f %f %f\n", (i-0.5)*dx, (j-0.5)*dy, uc/vscale*dx*mabiki, vc/vscale*dx*mabiki);
  }}
  fprintf(fp, "e \n");

  if(mabiki != 1){
    fprintf(fp, "plot '-' with vectors lc rgb 'black' \n");
    for(i=ni/5; i<=ni/5*4; i+= ni/10) {
    for(j=1; j<=nj; j++){
      uc = (u[i][j]+u[i-1][j])/2; //セル中心の速度
      vc = (v[i][j]+v[i][j-1])/2;
      fprintf(fp, "%f %f %f %f\n", (i-0.5)*dx, (j-0.5)*dy, uc/vscale*dx*mabiki, vc/vscale*dx*mabiki);
    }}
    fprintf(fp, "e \n");
  }

  fflush(fp);

  fprintf(fp, "unset multiplot \n");
}

//======================================================================
// 圧力分布の色塗り図（gnuplot） 
//======================================================================
void PlotPresure(){
  double x, y;
  int num_cnt;
  FILE *fp1 = _popen("C:/gnuplot/bin/gnuplot", "w"); 
  fprintf(fp1, "set terminal wxt title '圧力' size 400,400 \n");

  for(i=1;i<=ni;i++){
    p[i][0] = p[i][1];    // 下境界の圧力
    p[i][nj+1] = p[i][nj];  // 上境界の圧力
  }
  for(j=0;j<=nj+1;j++){
    p[0][j] = p[1][j];    // 左境界の圧力
    p[ni+1][j] = p[ni][j];  // 右境界の圧力
  }

  //描画設定
  fprintf(fp1, "set xrange [0:1]\n");
  fprintf(fp1, "set yrange [0:1]\n");
  fprintf(fp1, "set size ratio 1 \n");

  fprintf(fp1, "set palette defined ( 0 '#0000ff',1 '#00b8ff',2 '#00ffff',3 '#00ffb8',4 '#00ff00',5 '#b8ff00',6 '#ffff00',7 '#ffb800',8 '#ff0000') \n"); // レインボーカラー
  fprintf(fp1, "set pm3d map interpolate %d,%d \n", MAX(100/ni,1), MAX(100/nj,1));
  fprintf(fp1, "set contour \n");

  num_cnt=20;  // 等高線の本数
  fprintf(fp1, "set cntrparam levels %d \n", num_cnt); 

  fprintf(fp1, "set style line 1 lt 1 lw 1 lc rgb '#202020' \n");
  fprintf(fp1, "set style increment user \n");
  fprintf(fp1, "unset clabel \n");  //等高線を単色

  fprintf(fp1, "unset key \n");
//  fprintf(fp1, "unset tics \n");

  //描画
  fprintf(fp1, "splot '-' \n");
  for(i=0; i<=ni+1; i++) {
    for(j=0; j<=nj+1; j++){
      x=(i-0.5)*dx ; x=MAX(x,0.) ; x=MIN(x,Lx);
      y=(j-0.5)*dy ; y=MAX(y,0.) ; y=MIN(y,Ly);
      fprintf(fp1, "%f %f %f\n", x, y, p[i][j]);
    }
    fprintf(fp1, "\n");
  }
  fprintf(fp1, "e \n");
  fflush(fp1);
}

//======================================================================
// 流線図（流れ関数の等高線）（gnuplot）
//======================================================================
void PlotStreamline(){
  double s[ni+2][nj+2];   // 流れ関数
  double dummy, dd;
  int num_cnt;

  FILE *fp2 = _popen("C:/gnuplot/bin/gnuplot", "w"); 
  fprintf(fp2, "set terminal wxt title '流線' size 600,600 \n");

  //流れ関数

  for(i=0; i<=ni; i++){
    s[i][0] = 0; 
    for(j=1; j<=nj-1; j++){
      s[i][j] = s[i][j-1] + u[i][j]*dy;
    }
    s[i][nj] = 0;
  }

  smax = -1e6;
  smin = 1e6;
  for(i=0; i<=ni; i++){
  for(j=0; j<=nj; j++){
    smax = MAX(smax, s[i][j]);
    smin = MIN(smin, s[i][j]);
  }}


  //描画設定
  fprintf(fp2, "set xrange [0:1]\n");
  fprintf(fp2, "set yrange [0:1]\n");
  fprintf(fp2, "set size ratio 1 \n");  //set terminal size 800.800

  fprintf(fp2, "set pm3d map \n");
  fprintf(fp2, "set contour \n");

  num_cnt = 10;  // 等高線の本数
  dummy = MAX(fabs(smin),fabs(smax));

  //***********************************************
  // ra=1000000 の時はこれにする
  if(ra==1000000)
    dummy = dummy*0.973; // ra=1000000 の時は，中心付近の逆回転渦が見えるよう，このようにする。
  //-----------------------------------------------

  dd = dummy/num_cnt;
  fprintf(fp2, "set cntrparam levels incremental %20.12f, %20.12f, %20.12f \n", -dummy, dd, dummy); //流れ関数s=0の流線も書かせる。

  fprintf(fp2, "set style line 1 lt 1 lw 1 lc rgb '#000000' \n");
  fprintf(fp2, "set style increment user \n");
  fprintf(fp2, "unset clabel \n");  //等高線を単色

  fprintf(fp2, "set nosurface \n");
  fprintf(fp2, "unset key \n");
  fprintf(fp2, "unset tics \n");

  //ラベル
  fprintf(fp2, "set label 1 at graph 0.01,1.1 '流れ関数 ψmin=%f, ψmax=%f, Δψ=%f \n", smin, smax, dd);

  // 計算領域中心での流れ関数値を表示（Davisの無次元速度での値も表示）
  printf("s_mid=%f  ,  s_mid_Davis = %f \n", s[ni/2][nj/2], s[ni/2][nj/2]*pr*sqrt(gr));  

  //----------------------------------------------------------------------------------------------------------------
  // 流れ関数s=0の流線も書かせる場合に四隅の１セル内で無意味なs=0の流線を描かせないための処置（キャビティ内流れに固有の処置）
  if(s[1][nj-1]==0) dummy=0; else if(s[1][nj-1]>0) dummy=1e-10; else dummy=-1e-10;
  s[0][nj] = dummy ; s[1][nj] = dummy ; s[0][nj-1] = dummy;

  if(s[ni-1][nj-1]==0) dummy=0; else if(s[ni-1][nj-1]>0) dummy=1e-10; else dummy=-1e-10;
  s[ni][nj] = dummy ; s[ni-1][nj] = dummy ; s[ni][nj-1] = dummy;

  if(s[1][1]==0) dummy=0; else if(s[1][1]>0) dummy=1e-10; else dummy=-1e-10;
  s[0][0] = dummy ; s[1][0] = dummy ; s[0][1] = dummy ;

  if(s[ni-1][1]==0) dummy=0; else if(s[ni-1][1]>0) dummy=1e-10; else dummy=-1e-10;
  s[ni][0] = dummy ; s[ni-1][0] = dummy ; s[ni][1] = dummy ;
  //----------------------------------------------------------------------------------------------------------------

  // 描画
  fprintf(fp2, "splot '-' with lines\n");
  for(i=0; i<=ni; i++) {
    for(j=0; j<=nj; j++){
      fprintf(fp2, "%f %f %20.12f \n", i*dx, j*dy, s[i][j]);
      }
    fprintf(fp2, "\n");
  }
  fprintf(fp2, "e \n");

  fflush(fp2);
}

//======================================================================
// 温度分布の色塗り図（gnuplot） 
//======================================================================
void PlotT(){
  double x, y, dummy;
  int num_cnt;

  FILE *fp3 = _popen("C:/gnuplot/bin/gnuplot", "w"); 
  fprintf(fp3, "set terminal wxt title '温度' size 600,600 \n");

  for(i=1; i<=ni; i++){
    T[i][0] = T[i][1];      // 下境界 dT/dy = 0
    T[i][nj+1] = T[i][nj];  // 上境界 dT/dy = 0
  }
  for(j=0; j<=nj+1; j++){
    T[0][j] = 2*Th - T[1][j];     // 左境界 T=Th
    T[ni+1][j] = 2*Tl - T[ni][j];   // 右境界 T = Tl
  }

  //描画設定
  fprintf(fp3, "set xrange [0:1]\n");
  fprintf(fp3, "set yrange [0:1]\n");
  fprintf(fp3, "set size ratio 1 \n");

  fprintf(fp3, "set palette defined ( 0 '#0000ff',1 '#00b8ff',2 '#00ffff',3 '#00ffb8',4 '#00ff00',5 '#b8ff00',6 '#ffff00',7 '#ffb800',8 '#ff0000') \n"); // レインボーカラー
  fprintf(fp3, "set pm3d map interpolate %d,%d \n", MAX(100/ni,1), MAX(100/nj,1));

  fprintf(fp3, "set contour \n");
  fprintf(fp3, "set cntrparam cubicspline \n");
  num_cnt=10;  // 等高線の本数
  fprintf(fp3, "set cntrparam levels %d \n", num_cnt); 

  fprintf(fp3, "set style line 1 lt 1 lw 1 lc rgb '#202020' \n");
  fprintf(fp3, "set style increment user \n");
  fprintf(fp3, "unset clabel \n");  //等高線を単色

  fprintf(fp3, "unset key \n");
  fprintf(fp3, "unset tics \n");

  //描画
  fprintf(fp3, "splot '-' \n");
  for(i=0; i<=ni+1; i++) {
    for(j=0; j<=nj+1; j++){
      x=(i-0.5)*dx ; x=MAX(x,0.) ; x=MIN(x,Lx);
      y=(j-0.5)*dy ; y=MAX(y,0.) ; y=MIN(y,Ly);
      if(i==0) dummy = Th;
      else if(i==ni+1) dummy = Tl;
      else dummy = T[i][j];
      fprintf(fp3, "%f %f %f\n", x, y, dummy);
    }
    fprintf(fp3, "\n");
  }
  fprintf(fp3, "e \n");
  fflush(fp3);
}
