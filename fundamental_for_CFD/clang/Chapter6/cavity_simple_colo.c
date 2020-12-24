/************************************************
 cavity_simple_colo.cpp
 正方形キャビティ流れ（コロケート格子SIMPLE法による定常解の計算）
************************************************/

// fopen を fopen_s にすることを強要させない
#define _CRT_SECURE_NO_WARNINGS

//***** cavity_fvm_smac.cpp と cavity_simple.cpp で共通するもの *****
#include <stdio.h>
#include <math.h>
#define MAX(a,b) ((a)>(b)?(a):(b))
#define MIN(a,b) ((a)<(b)?(a):(b))

//#define ni 20         // 格子分割数（x方向）
//#define nj 20         // 格子分割数（y方向）

#define ni 40         // 格子分割数（x方向）
#define nj 40         // 格子分割数（y方向）

//#define ni 80         // 格子分割数（x方向）
//#define nj 80         // 格子分割数（y方向）

double u0= 1,                          // 上壁の速度
       Lx= 1, Ly= 1,                   // 計算領域の大きさ（x方向，y方向）

       re= 100,                        // レイノルズ数
       //re= 400,                        // レイノルズ数
       alpha= 1.8,                     // SORの加速係数
       converge= 1.e-6,                // SORの収束の判定基準

       u[ni+2][nj+2] , v[ni+2][nj+2],  // 速度
       p[ni+2][nj+2], pc[ni+2][nj+2],  // 圧力，圧力補正
       ae[ni+2][nj+2], aw[ni+2][nj+2], // 離散化方程式の係数 
       an[ni+2][nj+2], as[ni+2][nj+2],
       ap[ni+2][nj+2], b[ni+2][nj+2],
       dx, dy,                         // 格子サイズ
       visc,                           // 動粘性係数
       dvmax,                          // 速度vの変化量の最大値
       pcnew, err,
       smax,smin;                      // 流れ関数の最大値，最小値

int i, j, iter ;

void sor_pc();                         // 圧力補正のポアソン式をSOR法で解くルーチン
void PlotVector(), PlotStreamline(), PlotPresure(), SaveData();  // 結果の表示ルーチン等
FILE *fp ;                             // gnuplotとの接続のためのファイルポインタ

//**** それ以外のもの *****
double alpha_p = 0.8,              // 圧力補正の緩和係数
       alpha_u = 0.5,              // 速度場の緩和係数
       converge_outer= 1.e-5,      // 全体の反復の収束の判定基準
       uo[ni+2][nj+2], vo[ni+2][nj+2],  // 前の反復の速度
       aup[ni+2][nj+2],            // 速度uの離散化式の係数（注目点のみ）
       avp[ni+2][nj+2],            // 速度vの離散化式の係数（注目点のみ）

       ue[ni+2][nj+2], vn[ni+2][nj+2],   // セル界面の速度 
       d_u[ni+2][nj+2], d_v[ni+2][nj+2]  // 速度と圧力差の比例係数        
       ;
int n;                     // 反復回数
void coeff(); //離散化方程式の移流拡散の係数を求めるルーチン

//===========================
// メインプログラム
//===========================

int main() {
  //***** gnuplot と接続 *****
  fp = _popen("C:/gnuplot/bin/gnuplot", "w"); 
  fprintf(fp, "set terminal wxt title '速度ベクトル' size 600,600 \n");

  //***** 定数 *****
  visc = 1./re;
  dx = Lx/(double)ni ;
  dy = Ly/(double)nj ;

/*
  //***** 界面速度の初期値（初期速度分布が0以外の時は実行）*****
  for(i=1; i<=ni; i++){
  for(j=1; j<=nj; j++){
    if(i != ni) ue[i][j] = (u[i][j] + u[i+1][j])/2;
    if(j != nj) vn[i][j] = (v[i][j] + v[i][j+1])/2;
  }}
*/

  //**************************************************
  // 反復計算
  //**************************************************

do {
  n = n+1;
  for(i=1; i<=ni; i++){
  for(j=1; j<=nj; j++){
    uo[i][j] = u[i][j];
    vo[i][j] = v[i][j];
  }}

  //***** NS方程式 *****

  //-- 境界外側圧力 --
  for(i=1; i<=ni; i++){
    p[i][nj+1] = p[i][nj];
    p[i][0] = p[i][1];
  }
  for(j=1; j<=nj; j++){
    p[0][j] = p[1][j];
    p[ni+1][j] = p[ni][j];
  }

  //----- u -----
  // 離散化方程式の係数
  for(i=1; i<=ni; i++){
  for(j=1; j<=nj; j++){
    coeff();
    b[i][j] = (p[i-1][j]-p[i+1][j])/2*dy + (1-alpha_u)/alpha_u*ap[i][j]*uo[i][j];
    ap[i][j] = ap[i][j]/alpha_u;
    aup[i][j] = ap[i][j];  // 速度u についての係数apを保存（後で使う）

    // 接線方向速度の境界条件
    if(j==1){ap[i][j] += as[i][j]; as[i][j]=0;}
    if(j==nj){ap[i][j] += an[i][j]; b[i][j] += 2*an[i][j]*u0; an[i][j]=0;}

    // 法線方向速度の境界条件
    if(i==1){ap[i][j] += aw[i][j]; aw[i][j]=0;}
    if(i==ni){ap[i][j] += ae[i][j];  ae[i][j]=0;}
  }}

  // 速度uの反復計算（回数指定）
  for(iter=1; iter<=10; iter++){
    for(i=1; i<=ni; i++){
    for(j=1; j<=nj; j++){
    u[i][j] = (aw[i][j]*u[i-1][j] + ae[i][j]*u[i+1][j] + as[i][j]*u[i][j-1] + an[i][j]*u[i][j+1] + b[i][j])/ap[i][j];
  }}
  }

  //----- v -----
  // 離散化方程式の係数
  for(i=1; i<=ni; i++){
  for(j=1; j<=nj; j++){
    coeff();
    b[i][j] = (p[i][j-1]-p[i][j+1])/2*dx + (1-alpha_u)/alpha_u*ap[i][j]*vo[i][j];
    ap[i][j] = ap[i][j]/alpha_u;
    avp[i][j] = ap[i][j];  // 速度v についての係数apを保存（後で使う）

    // 接線方向速度の境界条件
    if(j==1){ap[i][j] += as[i][j]; as[i][j]=0;}
    if(j==nj){ap[i][j] += an[i][j]; an[i][j]=0;}

    // 法線方向速度の境界条件
    if(i==1){ap[i][j] += aw[i][j]; aw[i][j]=0;}
    if(i==ni){ap[i][j] += ae[i][j];  ae[i][j]=0;}
  }}

  // 速度vの反復計算（回数指定）
  for(iter=1; iter<=10; iter++){
    for(i=1; i<=ni; i++){
    for(j=1; j<=nj; j++){
    v[i][j] = (aw[i][j]*v[i-1][j] + ae[i][j]*v[i+1][j] + as[i][j]*v[i][j-1] + an[i][j]*v[i][j+1] + b[i][j])/ap[i][j];
  }}
  }
  //***** 圧力補正pc をSOR法で解く *****

  // 界面速度の補間
  for(i=1; i<ni; i++){           // i=ni は計算しないことに注意
  for(j=1; j<=nj; j++){
    d_u[i][j] = (dy/aup[i][j] + dy/aup[i+1][j])/2;
    ue[i][j] = (  u[i][j]   - (p[i-1][j]-p[i+1][j])/2*dy/aup[i][j]
                + u[i+1][j] - (p[i][j]  -p[i+2][j])/2*dy/aup[i+1][j] )/2
             + (p[i][j] - p[i+1][j])*d_u[i][j];
  }}

  for(i=1; i<=ni; i++){
  for(j=1; j<nj; j++){            // j=nj は計算しないことに注意
    d_v[i][j] = (dx/avp[i][j] + dx/avp[i][j+1])/2;
    vn[i][j] = (  v[i][j]   - (p[i][j-1]-p[i][j+1])/2*dx/avp[i][j]
                + v[i][j+1] - (p[i][j]  -p[i][j+2])/2*dx/avp[i][j+1] )/2
             + (p[i][j] - p[i][j+1])*d_v[i][j];
  }}

  // ポアソン方程式の係数
  for(i=1; i<=ni; i++){
  for(j=1; j<=nj; j++){
    aw[i][j] = dy*d_u[i-1][j];
    ae[i][j] = dy*d_u[i][j];
    as[i][j] = dx*d_v[i][j-1];
    an[i][j] = dx*d_v[i][j];
    ap[i][j] = aw[i][j] + ae[i][j] + as[i][j] + an[i][j];
    b[i][j] = -( (ue[i][j]-ue[i-1][j])*dy + (vn[i][j]-vn[i][j-1])*dx );
    pc[i][j] = 0;
  }}     //境界上では，(ue[i][j],d_u[i][j]やvn[i][j],d_v[i][j]が0となっていることに注意

  sor_pc();  // ポアソン方程式をSOR法で解く

  //***** 速度・圧力を修正する *****
  //-- 境界外側圧力 --
  for(i=1; i<=ni; i++){
    pc[i][nj+1] = pc[i][nj];
    pc[i][0] = pc[i][1];
  }
  for(j=1; j<=nj; j++){
    pc[0][j] = pc[1][j];
    pc[ni+1][j] = pc[ni][j];
  }

  dvmax = 0;
  for(i=1; i<=ni; i++){
  for(j=1; j<=nj; j++){
    u[i][j] = u[i][j] + (pc[i-1][j]-pc[i+1][j])/2*dy/aup[i][j];
    v[i][j] = v[i][j] + (pc[i][j-1]-pc[i][j+1])/2*dx/avp[i][j];
    p[i][j] = p[i][j] + alpha_p*pc[i][j];

    if(i != ni) ue[i][j] = ue[i][j] + (pc[i][j] - pc[i+1][j])*d_u[i][j];
    if(j != nj) vn[i][j] = vn[i][j] + (pc[i][j] - pc[i][j+1])*d_v[i][j];

    dvmax = MAX(dvmax, fabs(v[i][j]-vo[i][j]));
  }}

  //***** 途中結果の表示 *****
  printf("n=%d dvmax=%f\n",n,dvmax); 

  if(n % 200 ==0) PlotVector(); // 計算途中の速度ベクトル図を描く（計算の進行状況を見たいとき）

  if(n==1){
    PlotVector();
    printf("一時停止中。\n テキスト画面，グラフィック画面の位置や大きさを調節し，Enter キーを押して下さい。");
    getchar();
  }
}while (dvmax>converge_outer);

  //***** 計算終了後の表示 *****
  PlotVector();          // 速度ベクトル
  PlotPresure();         // 圧力分布
  PlotStreamline();      // 流線
  SaveData();            // データ保存
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
// 離散化方程式の移流拡散の係数
//**********************************************************************
void coeff(){
  // ue = ue[i][j], uw = ue[i-1][j]
  // vn = vn[i][j], vs = vn[j-1][j]

    //** 一次風上法 **
//    aw[i][j] = (MAX(ue[i-1][j],0) + visc/dx)*dy;
//    ae[i][j] = (MAX(-ue[i][j],0) + visc/dx)*dy;
//    as[i][j] = (MAX(vn[i][j-1],0) + visc/dy)*dx;
//    an[i][j] = (MAX(-vn[i][j],0) + visc/dy)*dx;

    //** 中心法 **
    aw[i][j] = (ue[i-1][j]/2 + visc/dx)*dy;
    ae[i][j] = (-ue[i][j]/2 + visc/dx)*dy;
    as[i][j] = (vn[i][j-1]/2 + visc/dy)*dx;
    an[i][j] = (-vn[i][j]/2 + visc/dy)*dx;

    ap[i][j] = aw[i][j] + ae[i][j] + as[i][j] + an[i][j];
}

//**********************************************************************
// 結果の保存
//**********************************************************************

// x=0.5 の垂直線上の速度uのy方向分布をcsvファイルで出力する

void SaveData(){
  FILE *fp3;
  fp3 = fopen("u_at_x_05.csv", "w");
  fprintf(fp3, "y , ue , (u(ni/2)+u(ni/2+1))/2 \n");
  fprintf(fp3, "%f , %f , %f \n",0.,0., 0.);
  for (j=1; j<=nj; j++){
    fprintf(fp3, "%f , %f , %f \n",dy*(j-0.5),ue[ni/2][j], (u[ni/2][j]+u[ni/2+1][j])/2 );
  }
  fprintf(fp3, "%f , %f , %f \n",Ly,1.,1.);
  fclose(fp3);
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

  if (ni >= 30) mabiki = ni/10;

  fprintf(fp, "set multiplot \n");
  fprintf(fp, "set xrange [0:1]\n");
  fprintf(fp, "set yrange [0:1]\n");
  fprintf(fp, "set size ratio 1 \n");
  fprintf(fp, "unset key \n");

  fprintf(fp, "plot '-' with vectors lc rgb 'black' \n");
  for(i=1; i<=ni; i+=mabiki) {
  for(j=1; j<=nj; j++){
    uc = u[i][j];
    vc = v[i][j];
    fprintf(fp, "%f %f %f %f\n", (i-0.5)*dx, (j-0.5)*dy, uc/u0*dx*mabiki, vc/u0*dx*mabiki);
  }}
  fprintf(fp, "e \n");

  if (mabiki != 1){
    fprintf(fp, "plot '-' with vectors lc rgb 'black' \n");
    for(j=1; j<=nj; j+=mabiki){
    for(i=1; i<=ni; i++) {
      uc = u[i][j];
      vc = v[i][j];
      fprintf(fp, "%f %f %f %f\n", (i-0.5)*dx, (j-0.5)*dy, uc/u0*dx*mabiki, vc/u0*dx*mabiki);
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
  double s[ni+2][nj+2];      // 流れ関数
  double dummy, dd;
  int num_cnt;

  FILE *fp2 = _popen("C:/gnuplot/bin/gnuplot", "w"); 
  fprintf(fp2, "set terminal wxt title '流線' size 600,600 \n");

  // 流れ関数

  for(i=0; i<=ni; i++){
    s[i][0] = 0; 
    for(j=1; j<=nj-1; j++){
      s[i][j] = s[i][j-1] + ue[i][j]*dy;
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

  // 描画設定
  fprintf(fp2, "set xrange [0:1]\n");
  fprintf(fp2, "set yrange [0:1]\n");
  fprintf(fp2, "set size ratio 1 \n");  //set terminal size 800.800

  fprintf(fp2, "set pm3d map \n");
  fprintf(fp2, "set contour \n");

  num_cnt = 10;                                                  // 等高線の本数
  dummy = MAX(fabs(smin),fabs(smax));
  dd = dummy/num_cnt;                                            // 等高線の間隔
  fprintf(fp2, "set cntrparam levels incremental %20.12f, %20.12f, %20.12f \n", -dummy, dd, dummy); // 流れ関数s=0の流線も書かせる。

  fprintf(fp2, "set style line 1 lt 1 lw 1 lc rgb '#000000' \n");
  fprintf(fp2, "set style increment user \n");
  fprintf(fp2, "unset clabel \n");                               // 等高線を単色

  fprintf(fp2, "set nosurface \n");
  fprintf(fp2, "unset key \n");
  fprintf(fp2, "unset tics \n");

  // ラベル
  fprintf(fp2, "set label 1 at graph 0.01,1.1 '流れ関数 ψmin=%f, ψmax=%f, Δψ=%f \n", smin, smax, dd);

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
