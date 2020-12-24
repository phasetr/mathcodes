/************************************************
body.cpp
 角柱まわりの流れ
 （SMAC法）

流出境界
　u[ni][j], v[ni+1][j] ：　uin で移流
  p[ni+1][j] = p[ni][j]

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

#define ni 250      // 格子分割数（x方向）
#define nj 50       // 格子分割数（y方向）

//#define ni 500      // 格子分割数（x方向）
//#define nj 100      // 格子分割数（y方向）

double uin= 1,        // 流入速度
       Lb = 1, Lx1 = 4,
       Lx= 25, Ly= 5,   // 計算領域の大きさ（x方向，y方向）
       re = 100,        // レイノルズ数
       alpha= 1.85,     // SORの加速係数(
       converge= 1.e-6,   // SORの収束の判定基準 （デフォルト）converge= 1.e-5 では渦の間隔がくずれる。
       cn,      // 時間きざみを決定する係数 (0<cn<1)

       u[ni+2][nj+2] , v[ni+2][nj+2], // 速度
       p[ni+2][nj+2], pc[ni+2][nj+2], // 圧力，圧力補正

       unew[ni+2][nj+2], vnew[ni+2][nj+2],  // 新しい時刻の速度
       flux_e[ni+2][nj+2],  // 検査体積の右界面の流束
       flux_n[ni+2][nj+2],  // 検査体積の上界面の流束

       ae[ni+2][nj+2], aw[ni+2][nj+2],  // 離散化方程式の係数
       an[ni+2][nj+2], as[ni+2][nj+2],
       ap[ni+2][nj+2], b[ni+2][nj+2],

       dx, dy,    // 格子サイズ
       visc,      // 動粘性係数
       dvmax,       // 速度vの変化量の最大値
       pcnew, err,
       smax,smin,

       ue,vn,       // セル界面の速度（右界面，上界面）
       time, dt,      // 時刻、時間きざみ
       dudt, dvdt, vold,
       dummy, v_x10, v_x10old, umean
       ;

int scheme;       // 1: 中心法
                  // 2: 一次風上
                  // 3: ハイブリッド

int n,            // 時刻ステップ
    ni1, ni2, nj1, nj2, i, j, iter,
    is_u_solve[ni+2][nj+2], is_v_solve[ni+2][nj+2], is_p_solve[ni+2][nj+2];

// 以下はFullDataSave で保存せず。
int nstep_full_data_save = 500;  // FullDataSave を行う間隔
//double endtime = 200;            // 計算終了時刻
double endtime = 120;            // 計算終了時刻

// ファイルポインタ
//FILE *fp  = _popen("C:/gnuplot/bin/gnuplot", "w");    // gnuplotとの接続のためのファイルポインタ
//FILE *fp1 = _popen("C:/gnuplot/bin/gnuplot", "w");
//FILE *fp2 = _popen("C:/gnuplot/bin/gnuplot", "w");
//FILE *fp3 = _popen("C:/gnuplot/bin/gnuplot", "w");
FILE *fp  = popen("/usr/bin/gnuplot", "w");    // gnuplotとの接続のためのファイルポインタ
FILE *fp1 = popen("/usr/bin/gnuplot", "w");
FILE *fp2 = popen("/usr/bin/gnuplot", "w");
FILE *fp3 = popen("/usr/bin/gnuplot", "w");
FILE *fp0 = fopen("v_at_x_10.csv", "w");

// 関数の型宣言
void sor_pc();  // 圧力補正のポアソン式をSOR法で解くルーチン
void PlotVector(), PlotStreamline(), PlotPresure(), PlotVorticity(), PlotVorticity1(), SaveData();  // 結果の表示ルーチン等
void FullDataSave(), FullDataLoad();

// 作業用
double v_o[ni+2][nj+2],  // 前の時刻の速度
       dd;
int cnt;

//===========================
// メインプログラム
//===========================
int main() {

// 中心法
  scheme = 1;
  cn = 0.1;
  //cn = 0.3; // 250x50メッシュの時，計算途中で発散

// 風上
//  scheme = 2;
//  cn = 0.5;

// ハイブリッド
//  scheme = 3;
//  cn = 0.5;

//*********************************************
// フルデータロード（以前の計算結果を読み込んでグラフィック表示，または継続計算する場合）
//---------------------------------------------
  //FullDataLoad(); printf("フルデータを読み込みました \n Enter キーを押して下さい \n"); getchar();
//---------------------------------------------


  //***** gnuplot と接続 ほか *****

  fprintf(fp , "set terminal wxt title '速度ベクトル' size 1000,300 \n");
  fprintf(fp1, "set terminal wxt title '圧力' size 1000,300 \n");
  fprintf(fp2, "set terminal wxt title '流線' size 1000,300 \n");
  fprintf(fp3, "set terminal wxt title '渦度' size 1000,300 \n");

/*
  // 大き目（本に掲載する図を取得時の設定）（クリップボードにコピーする時はwxtウィンドウを最大化すること）
  fprintf(fp , "set terminal wxt title '速度ベクトル' size 1200,400 \n");
  fprintf(fp1, "set terminal wxt title '圧力' size 1200,400 \n");
  fprintf(fp2, "set terminal wxt title '流線' size 1200,400 \n");
  fprintf(fp3, "set terminal wxt title '渦度' size 1200,400 \n");
*/

  fprintf(fp0,"time , v_at_x_10 \n"); // 角柱後方の x=10, y=Ly/2 の速度v の時間変化を記録（渦放出の周期が分かる）

  //***** 定数 *****
  visc = 1./re;
  dx = Lx/(double)ni ; dy = Ly/(double)nj ;
  dt = cn/(uin/dx + 2*visc*(1/dx/dx + 1/dy/dy));

  ni1 = (int)(Lx1/dx + 1e-6);
  ni2 = (int)((Lx1+Lb)/dx + 1e-6);
  nj1 = (int)((Ly/2-Lb/2)/dy + 1e-6);
  nj2 = (int)((Ly/2+Lb/2)/dy + 1e-6);

  printf("ni1=%d  ni2=%d  nj1=%d  nj2=%d \n", ni1,ni2,nj1,nj2);

  //***** 計算点 *****
  // 速度u
  for(i=1; i<=ni-1; i++){ for(j=1; j<=nj; j++){
    is_u_solve[i][j] = 1;
  }}
  for(i=ni1; i<=ni2; i++){ for(j=nj1+1; j<=nj2; j++){
    is_u_solve[i][j] = 0;
  }}

  // 速度v
  for(i=1; i<=ni; i++){ for(j=1; j<=nj-1; j++){
    is_v_solve[i][j] = 1;
  }}
  for(i=ni1+1; i<=ni2; i++){ for(j=nj1; j<=nj2; j++){
    is_v_solve[i][j] = 0;
  }}

  // 圧力p
  for(i=1; i<=ni; i++){ for(j=1; j<=nj; j++){
    is_p_solve[i][j] = 1;
  }}
  for(i=ni1+1; i<=ni2; i++){ for(j=nj1+1; j<=nj2; j++){
    is_p_solve[i][j] = 0;
  }}

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

  // 角柱に隣接するセル
  for(i=ni1+1; i<=ni2; i++){
    j=nj1;
    ap[i][j] = ap[i][j] - an[i][j] ; an[i][j] = 0;
    j=nj2+1;
    ap[i][j] = ap[i][j] - as[i][j] ; as[i][j] = 0;
  }

  for(j=nj1+1; j<=nj2; j++){
    i=ni1;
    ap[i][j] = ap[i][j] - ae[i][j] ; ae[i][j] = 0;
    i=ni2+1;
    ap[i][j] = ap[i][j] - aw[i][j] ; aw[i][j] = 0;
  }

  //***** 流入境界における速度 *****
  i=0;
  for(j=1; j<=nj; j++) {
    u[i][j] = uin; unew[i][j] = uin;
  }

  //***** 初期速度場 *****
  if(n==0){
    for(i=1; i<=ni-1; i++){
    for(j=1; j<=nj; j++){
    if(is_u_solve[i][j]){
      if( (i>=ni1) && (i<=ni2) )
        u[i][j] = uin*Ly/(Ly-Lb);
      else
        u[i][j] = uin;
    }}}

    for(j=1; j<=nj; j++) u[ni][j] = uin;
  }

//*********************************************
// 以前の計算結果をフルデータロード後の描画
//---------------------------------------------
  //PlotStreamline();
  //PlotVorticity();
  //PlotVector();
  //PlotPresure();
  //printf("Enter キーを押して下さい \n"); getchar();
//---------------------------------------------

  //**************************************************
  // 時間前進
  //**************************************************

do {
  n = n+1;
  time = time + dt;

  if(n==1) {
    printf("この計算は時間がかかるので，Visual C++ の場合，Releaseモードで実行して下さい（Debugモードは遅いので） \n");
    printf("最初は圧力のポアソン式に時間がかかるので，しばらくお待ち下さい \n");
  }

  //***** 境界条件 *****
  for(i=1; i<=ni; i++){
    u[i][0] = u[i][1];      // 下境界 du/dy=0
    u[i][nj+1] = u[i][nj];  // 上境界 du/dy=0
  }
  for(j=1; j<=nj; j++){
    v[0][j] = -v[1][j];     // 左境界 v=0
    p[ni+1][j] = p[ni][j];  // 右境界 dp/dx=0
  }

  //***** NS方程式：次の時刻の速度の仮の値 *****

  //----- unew -----

  // 流束
  for(i=0; i<=ni; i++){
  for(j=0; j<=nj; j++){
    ue = (u[i][j]+u[i+1][j])/2;
    vn = (v[i][j]+v[i+1][j])/2;

    if(scheme == 1){
      //** 中心 **
      flux_e[i][j] = ue*(u[i+1][j]+u[i][j])/2 - visc/dx*(u[i+1][j]-u[i][j]);              //中心
      flux_n[i][j] = vn*(u[i][j+1]+u[i][j])/2 - visc/dy*(u[i][j+1]-u[i][j]);              //中心
    }
    else if(scheme == 2){
      //** 一次風上 **
      flux_e[i][j] = ue*(u[i+1][j]+u[i][j])/2 - (visc/dx + fabs(ue)/2)*(u[i+1][j]-u[i][j]); //１次風上
      flux_n[i][j] = vn*(u[i][j+1]+u[i][j])/2 - (visc/dy + fabs(vn)/2)*(u[i][j+1]-u[i][j]); //１次風上
    }
    else{
      //** ハイブリッド **
      flux_e[i][j] = ue*(u[i+1][j]+u[i][j])/2 - MAX(fabs(ue)/2, visc/dx)*(u[i+1][j]-u[i][j]); //ハイブリッド
      flux_n[i][j] = vn*(u[i][j+1]+u[i][j])/2 - MAX(fabs(vn)/2, visc/dy)*(u[i][j+1]-u[i][j]); //ハイブリッド
    }
  }}

  //角柱の上下面の処理
  for(i=ni1; i<=ni2; i++){
    j=nj1;
    flux_n[i][j] = visc/dy*2*u[i][j];
    j=nj2;
    flux_n[i][j] = -visc/dy*2*u[i][j+1];
  }

  // 次の時刻の値
  for(i=1; i<=ni; i++){
  for(j=1; j<=nj; j++){
  if(is_u_solve[i][j]){
    dudt = (flux_e[i-1][j]-flux_e[i][j])/dx + (flux_n[i][j-1]-flux_n[i][j])/dy;
    unew[i][j] = u[i][j]+dudt*dt  + (p[i][j]-p[i+1][j])/dx*dt;
  }}}

  //----- vnew -----

  // 流束
  for(i=0; i<=ni; i++){
  for(j=0; j<=nj-1; j++){
    ue = (u[i][j]+u[i][j+1])/2;
    vn = (v[i][j]+v[i][j+1])/2;

    if(scheme == 1){
     //** 中心 **
      flux_e[i][j] = ue*(v[i+1][j]+v[i][j])/2 - visc/dx*(v[i+1][j]-v[i][j]);              //中心
      flux_n[i][j] = vn*(v[i][j+1]+v[i][j])/2 - visc/dy*(v[i][j+1]-v[i][j]);              //中心
    }
    else if( scheme == 2){
      //** 一次風上 **
      flux_e[i][j] = ue*(v[i+1][j]+v[i][j])/2 - (visc/dx + fabs(ue)/2)*(v[i+1][j]-v[i][j]); //１次風上
      flux_n[i][j] = vn*(v[i][j+1]+v[i][j])/2 - (visc/dy + fabs(vn)/2)*(v[i][j+1]-v[i][j]); //１次風上
    }
    else {
      //** ハイブリッド **
      flux_e[i][j] = ue*(v[i+1][j]+v[i][j])/2 - MAX(visc/dx, fabs(ue)/2)*(v[i+1][j]-v[i][j]); // ハイブリッド
      flux_n[i][j] = vn*(v[i][j+1]+v[i][j])/2 - MAX(visc/dy, fabs(vn)/2)*(v[i][j+1]-v[i][j]); // ハイブリッド
    }
  }}

  //角柱の上下面の処理
  for(j=nj1; j<=nj2; j++){
    i=ni1;
    flux_e[i][j] = visc/dx*2*v[i][j];
    i=ni2;
    flux_e[i][j] = -visc/dx*2*v[i+1][j];
  }

  // 次の時刻の値
  for(i=1; i<=ni; i++){
  for(j=1; j<=nj-1; j++){
  if(is_v_solve[i][j]){
    dvdt = (flux_e[i-1][j]-flux_e[i][j])/dx + (flux_n[i][j-1]-flux_n[i][j])/dy;
    vnew[i][j] = v[i][j]+dvdt*dt + (p[i][j]-p[i][j+1])/dy*dt;
  }}}

  //----- 流出境界の速度u -----
  umean = 0;
  for(j=1; j<=nj; j++){
    unew[ni][j] = u[ni][j]-uin*(u[ni][j]-u[ni-1][j])/dx*dt;
    umean += unew[ni][j];
  }
  umean = umean/nj;
  for(j=1; j<=nj; j++){
    if (umean !=0) unew[ni][j] = unew[ni][j]/umean*uin;
    else unew[ni][j] += uin;
  }
  //printf("n=%d  umean=%f \n",n,umean); getchar();

  //***** 速度のdivergence *****
  for(i=1; i<=ni; i++){
  for(j=1; j<=nj; j++){
    b[i][j] = -( (unew[i][j]-unew[i-1][j])/dx + (vnew[i][j]-vnew[i][j-1])/dy )/dt;
    pc[i][j] = 0;
  }}

  //***** 圧力補正pcをSOR法で解く *****
  sor_pc();

  //***** 速度・圧力を修正し，前の値を更新する *****
  dvmax = 0.;
  for(i=1; i<=ni; i++){
  for(j=1; j<=nj; j++){
    v_o[i][j] = v[i][j];

    vold = v[i][j];
    if(is_u_solve[i][j]) u[i][j] = unew[i][j] + (pc[i][j]-pc[i+1][j])/dx*dt;
    if(is_v_solve[i][j]) v[i][j] = vnew[i][j] + (pc[i][j]-pc[i][j+1])/dy*dt;
    p[i][j] = p[i][j] + pc[i][j];
    dvmax = MAX(dvmax, fabs(v[i][j]-vold));
  }}

  // 流出境界速度
  for(j=1; j<=nj-1; j++){
    v[ni+1][j] = v[ni+1][j]-uin*(v[ni+1][j]-v[ni][j])/dx*dt;
  }
  for(j=1; j<=nj; j++){
    u[ni][j] = unew[ni][j];
  }

  //***** 途中結果の表示 *****
  printf("n=%d time=%f iter=%d dvmax=%f\n",n,time, iter,dvmax);

  if(n==1) {
    PlotStreamline();
    PlotVorticity();
    PlotVector();
    PlotPresure();
    printf("一時停止中。\n テキスト画面，グラフィック画面の位置や大きさを調節し，Enter キーを押して下さい。");
    getchar();
  }

  if(n%500 == 0) {   // 計算途中の速度ベクトル図を描く（計算の進行状況を見たいとき）
    PlotStreamline();
    PlotVorticity();
    PlotVector();
    PlotPresure();
  }

  // x=10, y=Ly/2 における速度vを時間0.1毎に記録（後でストローハル数を求めることができる）
  if (int((time-dt)/0.1) != int(time/0.1)){
    i = int((10.+1.e-6)/dx); j = nj/2;
    dummy =(v[i][j]+v[i+1][j])/2;
    fprintf(fp0,"%f , %f \n", time, dummy);
  }

  // 定期的にフルデータ保存
  if( (n % nstep_full_data_save) ==0){
    FullDataSave();
    printf("******* Full Data Saved, n=%d \n",n);
  }

  //  x=10, y=Ly/2 における速度vが符号を変えた時，物体後方の中心線上の速度vの分布を保存する
  v_x10 = (v[ni2*2][nj/2]+v[ni2*2+1][nj/2])/2;
  if( time > 100.){
    if ( ((v_x10old<0) && (v_x10>0)) ||
         ((v_x10old>0) && (v_x10<0))   ){
      SaveData();
      printf("******* v_x10 data saved \n");
    }
  }
  v_x10old = v_x10;

/*
// 本に掲載する図を取るため，t=100 まで t=10毎に一時停止
  for(cnt=10; cnt<=100; cnt += 10){
    if((time-dt)<(double)cnt && time>=(double)cnt ){
      printf("Hit enter key \n"); getchar();
    }
  }
*/

/*
// 本に掲載するスナップショットに適したタイミングを探すのに使ったルーチン。
// その結果，i=138 の速度v(j=nj/2)が符号を変える時にスナップショットをとることに決定（250x50の場合）
  if( time > 100.){
    for(i=ni2*2; i<=ni2*2+int(5/dx); i+=2){
      if  ( (v_o[i][nj/2]<0) &&  (v[i][nj/2]>0) ){
        printf("v が 0クロスするi=%d \n Hit enter key \n", i);
        PlotStreamline();
        PlotVorticity();
        PlotPresure();
        FullDataSave();
        getchar();
        break;
      }
    }
  }
*/


// 本に掲載するスナップショット用にフルデータ保存
  if( time > 100.){
    if  ( (v_o[138][nj/2]<0) &&  (v[138][nj/2]>0) ){
      printf("v[138][nj/2] が 0クロスした \n （一時停止中） \n" );
      PlotStreamline();
      PlotVorticity();
      PlotPresure();
      FullDataSave();
      getchar();
    }
  }


}while (time<endtime);

  //***** 計算終了後の表示 *****

  PlotStreamline();
  PlotVorticity();
  PlotVector();
  PlotPresure();
  FullDataSave();
  fclose(fp0);

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
    if(is_p_solve[i][j]){
      pcnew = (aw[i][j]*pc[i-1][j]+ae[i][j]*pc[i+1][j]
              +as[i][j]*pc[i][j-1]+an[i][j]*pc[i][j+1]+b[i][j])/ap[i][j] ;
      err = MAX(err, fabs(pcnew-pc[i][j]));
      pc[i][j] = pc[i][j] + alpha*(pcnew-pc[i][j]);
    }}}
    if(iter%10000 == 0) {
      printf("iter=%d err=%f \n",iter, err);
//      getchar();
    }
  } while (err>converge);
}

//**********************************************************************
// 結果の保存
//**********************************************************************

// 角柱中心線上の速度vのx方向分布をcsvファイルで出力する

void SaveData(){
  FILE *fp;
  char name[21];

  sprintf(name, "v_along_x_%06d.csv",n);

  fp = fopen(name, "w");
  fprintf(fp, "%d , %f \n",n,time);
  fprintf(fp, "x , v \n");
  for (i=1; i<=ni; i++) fprintf(fp, "%f , %f \n",dx*(i-0.5),v[i][nj/2]);
  fclose(fp);
}





//**********************************************************************
// 結果のグラフィック表示（gnuplot）
//**********************************************************************

//======================================================================
// 速度ベクトル図（gnuplot）
//======================================================================
void PlotVector(){
  double uc, vc;
  int i,j;
  int mabiki=1;

  mabiki = Lb/dx;

  fprintf(fp, "set multiplot \n");

  fprintf(fp, "set xrange [0:%f]\n", Lx);
  fprintf(fp, "set yrange [0:%f]\n", Ly);
  fprintf(fp, "set size ratio %f \n", Ly/Lx);
  fprintf(fp, "unset key \n");

  fprintf(fp, "set label 1 at graph 0.01,1.1 '速度ベクトル  n=%d time=%f \n", n,time);

  fprintf(fp, "plot '-' with vectors lc rgb 'black' \n");
  for(i=mabiki/2+1; i<=ni; i+=mabiki) {
    for(j=1; j<=nj; j+=2){
      uc = (u[i][j]+u[i-1][j])/2; //セル中心の速度
      vc = (v[i][j]+v[i][j-1])/2;
      fprintf(fp, "%f %f %f %f\n", (i-0.5)*dx, (j-0.5)*dy, uc/uin*dx*mabiki*0.6, vc/uin*dx*mabiki*0.6);
    }
  }
  fprintf(fp, "e \n");

  //-----------------------------------------------------
  //***** 角柱を描く *****
  fprintf(fp, "plot '-' with lines lc rgb 'red' \n");
  fprintf(fp, "%f %f \n", Lb*4,Lb*2);
  fprintf(fp, "%f %f \n", Lb*4,Lb*3);
  fprintf(fp, "%f %f \n", Lb*5,Lb*3);
  fprintf(fp, "%f %f \n", Lb*5,Lb*2);
  fprintf(fp, "%f %f \n", Lb*4,Lb*2);
  fprintf(fp, "e \n");

  fflush(fp);

  fprintf(fp, "unset multiplot \n");

}



//======================================================================
// 圧力分布の色塗り図（gnuplot）
//======================================================================
void PlotPresure(){
  double x, y, pmax, pmin, dd;
  int i,j, num_cnt;

  for(i=1;i<=ni;i++){
    p[i][0] = p[i][1];    // 下境界の圧力
    p[i][nj+1] = p[i][nj];  // 上境界の圧力
  }
  for(j=0;j<=nj+1;j++){
    p[0][j] = p[1][j];    // 左境界の圧力
    p[ni+1][j] = p[ni][j];  // 右境界の圧力  // XXXXXXXXXXXX 右境界 dp/dx=0  XXXXXXXXXXXXXXXXXXXX
  }

  for(i=ni1+1; i<=ni2; i++){
    p[i][nj1+1] = p[i][nj1];
    p[i][nj2] = p[i][nj2+1];
  }
  for(j=nj1+1;j<=nj2; j++){
    p[ni1+1][j] = p[ni1][j];
    p[ni2][j] = p[ni2+1][j];
  }

  pmax = -1e6;
  pmin = 1e6;
  for(i=1; i<=ni; i++){
  for(j=1; j<=nj; j++){
  if(is_p_solve[i][j]){
    pmax = MAX(pmax, p[i][j]);
    pmin = MIN(pmin, p[i][j]);
  }}}
  printf("pmax=%f  pmin=%f \n",pmax, pmin);

  //***** 描画の設定 *****
  fprintf(fp1, "set multiplot \n");
  fprintf(fp1, "set colorbox \n");  //カラーバーを描く

  fprintf(fp1, "set xrange [0:%f]\n", Lx);
  fprintf(fp1, "set yrange [0:%f]\n", Ly);
  fprintf(fp1, "set size ratio %f \n", Ly/Lx);

  fprintf(fp1, "set palette defined ( 0 '#0000ff',1 '#00b8ff',2 '#00ffff',3 '#00ffb8',4 '#00ff00',5 '#b8ff00',6 '#ffff00',7 '#ffb800',8 '#ff0000') \n"); // レインボーカラー
  fprintf(fp1, "set pm3d map \n");  // map -> 3D を上から見た図のみ（サーフェスプロット）。set size ratio が有効になる。
  fprintf(fp1, "set contour \n");  // 等高線を描く

  fprintf(fp1, "set zrange [%f:%f]\n", pmin,pmax);
  fprintf(fp1, "set cbrange[%f:%f] \n", pmin, pmax);  // カラーバーの範囲

  //等高線の下限，上限，間隔
  dd = 0.05;
  fprintf(fp1, "set cntrparam levels incremental -2.,%f,2. \n",dd);  //等高線の下限，間隔，上限

//  num_cnt = 40; //等高線の数
//  dd = (pmax-pmin)/num_cnt;   // 等高線の間隔
//  fprintf(fp1, "set cntrparam levels incremental %f,%f,%f \n",pmin,dd,pmax);  //等高線の下限，間隔，上限

//  fprintf(fp1, "set cntrparam levels %d \n",num_cnt);

  //等高線の線の色を単色
  fprintf(fp1, "set style line 1 lt 1 lw 1 lc rgb '#202020' \n");
  fprintf(fp1, "set style increment user \n");
  fprintf(fp1, "unset clabel \n");

  //その他設定
  fprintf(fp1, "unset key \n");
//  fprintf(fp1, "unset tics \n");

  //ラベル
  fprintf(fp1, "set label 1 at graph 0.01,1.1 '圧力  n=%d time=%f (Δp=%f) \n", n,time, dd);

  //***** 描画 *****
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

  //-----------------------------------------------------
  //***** 角柱を塗りつぶす *****

  fprintf(fp1, "set surface \n");
  fprintf(fp1, "unset contour \n");
  fprintf(fp1, "set zrange [-1:1]\n");
  fprintf(fp1, "unset colorbox \n");
  fprintf(fp1, "set palette defined (-1 '#b0b0b0', 0 '#b0b0b0', 1 '#b0b0b0') \n"); //灰色

  fprintf(fp1, "splot '-' \n");
  for(i=ni1; i<=ni2; i+=(ni2-ni1)) {
    for(j=nj1; j<=nj2; j+=(nj2-nj1)){
      fprintf(fp1, "%f %f %f\n", i*dx, j*dy, 0.);
      }
    fprintf(fp1, "\n");
  }
  fprintf(fp1, "e \n");

  //---------------------------
  fflush(fp1);
  fprintf(fp1, "unset multiplot \n");

}

//======================================================================
// 流線図（流れ関数の等高線）（gnuplot）
//======================================================================
void PlotStreamline(){
  double s[ni+2][nj+2];   // 流れ関数
  int i,j, num_cnt;

  //***** 流れ関数 *****
  for(i=0; i<=ni; i++){
    s[i][0] = 0;
    for(j=1; j<=nj; j++){
      s[i][j] = s[i][j-1] + u[i][j]*dy;
    }
  }

  smax = -1e6;
  smin = 1e6;
  for(i=0; i<=ni; i++){
  for(j=0; j<=nj; j++){
    smax = MAX(smax, s[i][j]);
    smin = MIN(smin, s[i][j]);
  }}


  //***** 描画の設定 *****
  fprintf(fp2, "set multiplot \n");

  fprintf(fp2, "set xrange [0:%f]\n",Lx);
  fprintf(fp2, "set yrange [0:%f]\n", Ly);
  fprintf(fp2, "set size ratio 0.2 \n");

  fprintf(fp2, "set zrange [%f:%f]\n", smin,smax);

  fprintf(fp2, "set pm3d map \n");
  fprintf(fp2, "set contour \n");
  num_cnt=30;
  //num_cnt=20;
  fprintf(fp2, "set cntrparam levels incremental %f,%f,%f \n", 0., uin*Ly/num_cnt,uin*Ly); //等高線の下限，間隔，上限

  // 等高線の線の色を単色
  fprintf(fp2, "set style line 1 lt 1 lw 1 lc rgb '#000000' \n");
  fprintf(fp2, "set style increment user \n");
  fprintf(fp2, "unset clabel \n");

  // その他設定
  fprintf(fp2, "unset key \n");
  fprintf(fp2, "unset tics \n");
  fprintf(fp2, "set nosurface \n");

  // ラベル
  fprintf(fp2, "set label 1 at graph 0.01,1.1 '流線  n=%d time=%f \n", n,time);

  //***** 描画 *****
  fprintf(fp2, "splot '-' with lines\n");
  for(i=0; i<=ni; i++) {
    for(j=0; j<=nj; j++){
      fprintf(fp2, "%f %f %f\n", i*dx, j*dy, s[i][j]);
      }
    fprintf(fp2, "\n");
  }
  fprintf(fp2, "e \n");

  //-----------------------------------------------------
  //***** 角柱を塗りつぶす *****

  fprintf(fp2, "set surface \n");
  fprintf(fp2, "unset contour \n");
  fprintf(fp2, "set zrange [-1:1]\n");
  fprintf(fp2, "set cbrange [-1:1] \n");
  fprintf(fp2, "unset colorbox \n");
  fprintf(fp2, "set palette defined (-1 '#b0b0b0', 0 '#b0b0b0', 1 '#b0b0b0') \n"); //灰色

  fprintf(fp2, "splot '-' \n");
  for(i=ni1; i<=ni2; i+=(ni2-ni1)) {
    for(j=nj1; j<=nj2; j+=(nj2-nj1)){
      fprintf(fp2, "%f %f %f\n", i*dx, j*dy, 0.);
      }
    fprintf(fp2, "\n");
  }
  fprintf(fp2, "e \n");

  //---------------------------

  fflush(fp2);
  fprintf(fp2, "unset multiplot \n");
}

//======================================================================
// 渦度の等高線（gnuplot）
//======================================================================

void PlotVorticity(){
  double vorticity[ni+2][nj+2];   // 渦度
  double vor_max, vor_min;
  int i,j;

  //***** 渦度 *****

  for(i=0; i<=ni; i++){
    u[i][0] = u[i][1];      // 下境界 du/dy=0
    u[i][nj+1] = u[i][nj];  // 上境界 du/dy=0
  }
  for(j=1; j<=nj; j++){
    v[0][j] = -v[1][j];     // 左境界 v=0
  }

  for(i=0; i<=ni; i++){
  for(j=0; j<=nj; j++){
    vorticity[i][j] = -(u[i][j+1]-u[i][j])/dy + (v[i+1][j]-v[i][j])/dx;
  }}

  for(i=ni1; i<=ni2; i++){
    j=nj2; vorticity[i][j] = -u[i][j+1]/dy*2;
    j=nj1; vorticity[i][j] = u[i][j]/dy*2;
  }

  for(j=nj1; j<=nj2; j++){
    if( (j==nj1) || (j==nj2) ){
      i=ni2; vorticity[i][j] = (vorticity[i][j] + v[i+1][j]/dx*2)/2;  //角部
      i=ni1; vorticity[i][j] = (vorticity[i][j] - v[i][j]/dx*2)/2;  //角部
    }
    else{
      i=ni2; vorticity[i][j] = v[i+1][j]/dx*2;
      i=ni1; vorticity[i][j] = -v[i][j]/dx*2;
    }
  }

  vor_max = -1e6;
  vor_min = 1e6;
  for(i=0; i<=ni; i++){
  for(j=0; j<=nj; j++){
    vor_max= MAX(vorticity[i][j] , vor_max);
    vor_min= MIN(vorticity[i][j] , vor_min);
  }}
  printf("vor_max=%f vor_min=%f \n", vor_max, vor_min);

  //***** 描画の設定 *****
  fprintf(fp3, "set multiplot \n");
  fprintf(fp3, "set colorbox \n");  //カラーバーを描く

  fprintf(fp3, "set xrange [0:%f]\n",Lx);
  fprintf(fp3, "set yrange [0:%f]\n", Ly);
  fprintf(fp3, "set size ratio %f \n", Ly/Lx);

  fprintf(fp3, "set palette defined (0 'blue', 1 'white', 2 'red') \n");  // 赤・白・黒
  fprintf(fp3, "set pm3d map \n");  // map -> 3D を上から見た図のみ（サーフェスプロット）。set size ratio が有効になる。
  fprintf(fp3, "set contour \n");  // 等高線を描く

  //等高線の下限，上限，間隔
  dummy = MAX(fabs(vor_min), fabs(vor_max));
  dd = 0.5;                                   // 等高線の間隔
  dummy = (int(dummy/dd)+1)*dd;
  fprintf(fp3, "set zrange [%f:%f]\n", -dummy,dummy);   // 渦度0の等渦度線が描かれるよう工夫
  fprintf(fp3, "set cntrparam levels incremental %f,%f,%f \n", -dummy, dd,dummy);  //等高線の下限，間隔，上限
  fprintf(fp3, "set cbrange[%f:%f] \n", -dummy,dummy);  // カラーバーの範囲

  //等高線の線の色を単色
  fprintf(fp3, "set style line 1 lt 1 lw 1 lc rgb '#000000' \n");
  fprintf(fp3, "set style increment user \n");
  fprintf(fp3, "unset clabel \n");


  //その他設定
  fprintf(fp3, "unset key \n");
//  fprintf(fp3, "unset tics \n");
//  fprintf(fp3, "set nosurface \n"); //この文を実行すると色塗りがされない

  //ラベル
  fprintf(fp3, "set label 1 at graph 0.01,1.1 '渦度  n=%d time=%f  (Δω=%f) \n", n,time,dd );

  //***** 描画 *****
  fprintf(fp3, "splot '-' \n");
  for(i=0; i<=ni; i++) {
    for(j=0; j<=nj; j++){
      fprintf(fp3, "%f %f %f\n", i*dx, j*dy, vorticity[i][j]);
      }
    fprintf(fp3, "\n");
  }
  fprintf(fp3, "e \n");

  //-----------------------------------------------------
  //***** 角柱を塗りつぶす *****

  fprintf(fp3, "set surface \n");
  fprintf(fp3, "unset contour \n");
  fprintf(fp3, "set zrange [-1:1]\n");
  fprintf(fp3, "unset colorbox \n");
  fprintf(fp3, "set palette defined (-1 '#b0b0b0', 0 '#b0b0b0', 1 '#b0b0b0') \n"); //灰色

  fprintf(fp3, "splot '-' \n");
  for(i=ni1; i<=ni2; i+=(ni2-ni1)) {
    for(j=nj1; j<=nj2; j+=(nj2-nj1)){
      fprintf(fp3, "%f %f %f\n", i*dx, j*dy, 0.);
      }
    fprintf(fp3, "\n");
  }
  fprintf(fp3, "e \n");

  //---------------------------
  fflush(fp3);
  fprintf(fp3, "unset multiplot \n");
}

//**********************************************************************
// 計算を再開できるフルデータの保存と読み込み（バイナリデータ）
//**********************************************************************

//===========================
// フルデータの保存
//===========================

void FullDataSave(){
  int nni, nnj, num;
  FILE *fp;

  fp = fopen( "FullData.dat", "wb" );

  nni=ni; nnj=nj;
  fwrite( &nni, sizeof(int), 1, fp );
  fwrite( &nnj, sizeof(int), 1, fp );

  fwrite( &uin, sizeof(double), 1, fp );
  fwrite( &Lb , sizeof(double), 1, fp );
  fwrite( &Lx1, sizeof(double), 1, fp );
  fwrite( &Lx , sizeof(double), 1, fp );
  fwrite( &Ly , sizeof(double), 1, fp );
  fwrite( &re , sizeof(double), 1, fp );
  fwrite( &alpha , sizeof(double), 1, fp );
  fwrite( &converge , sizeof(double), 1, fp );
  fwrite( &scheme, sizeof(int), 1, fp );

  fwrite( &n, sizeof(int), 1, fp );
  fwrite( &time, sizeof(double), 1, fp );

  num = (ni+2)*(nj+2);
  fwrite( u, sizeof(double), num, fp );
  fwrite( v, sizeof(double), num, fp );
  fwrite( p, sizeof(double), num, fp );

  fclose( fp );
}

//===========================
// フルデータの読み込み
//===========================

void FullDataLoad(){
  int nni, nnj, num;
  FILE *fp;

  fp = fopen( "FullData.dat", "rb" );

  fread( &nni, sizeof(int), 1, fp );
  fread( &nnj, sizeof(int), 1, fp );
  fread( &uin, sizeof(double), 1, fp );
  fread( &Lb , sizeof(double), 1, fp );
  fread( &Lx1, sizeof(double), 1, fp );
  fread( &Lx , sizeof(double), 1, fp );
  fread( &Ly , sizeof(double), 1, fp );
  fread( &re , sizeof(double), 1, fp );
  fread( &alpha , sizeof(double), 1, fp );
  fread( &converge , sizeof(double), 1, fp );
  fread( &scheme, sizeof(int), 1, fp );

  fread( &n, sizeof(int), 1, fp );
  fread( &time, sizeof(double), 1, fp );

  printf("nni=%d nnj=%d \n", nni,nnj);
  printf("ni =%d nj =%d \n", ni,nj);
  printf("n =%d time =%f \n", n,time);


  if( (nni != ni) || (nnj != nj) ) {
    printf(" (nni != ni) or (nnj != nj) !!! \n");
    getchar();
  }

  num = (nni+2)*(nnj+2);
  fread( u, sizeof(double), num, fp );
  fread( v, sizeof(double), num, fp );
  fread( p, sizeof(double), num, fp );

  fclose( fp );

}
