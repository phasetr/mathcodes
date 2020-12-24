/************************************************
body_heated.cpp
 入口・出口が開放端の鉛直流路内の加熱角柱からの自然対流
 （SMAC法）


流出境界
  圧力一定
  u,v,T 流れ方向（x方向）勾配0

流入境界
　全圧一定
  du/dx=0, v=0, T=Tin

流路壁
  すべりなし，断熱

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

#define ni 150      // 格子分割数（x方向）
#define nj 30       // 格子分割数（y方向）

double Lb = 1, Lx1 = 6,
       Lx= 15, Ly= 3,   // 計算領域の大きさ（x方向，y方向）

       ra= 1000,        // レイリー数
       pr = 0.71,         // プラントル数

       alpha= 1.85,     // SORの加速係数(
       converge= 1.e-6,   // SORの収束の判定基準 （デフォルト）converge= 1.e-5 では渦の間隔がくずれる。
       cn,      // 時間きざみを決定する係数 (0<cn<1)

			 // 以下は固定値
			 Tb = 1,            // 物体温度
       Tin = 0,           // 入口温度
			 pin = 0,           // 入口圧力
			 pout = 0,          // 出口圧力

			 u[ni+2][nj+2] , v[ni+2][nj+2],       // 速度
       unew[ni+2][nj+2], vnew[ni+2][nj+2],  // 新しい時刻の速度
       p[ni+2][nj+2], pc[ni+2][nj+2],       // 圧力，圧力補正
       T[ni+2][nj+2], Tnew[ni+2][nj+2],     // 温度

       flux_e[ni+2][nj+2],  // 検査体積の右界面の流束
       flux_n[ni+2][nj+2],  // 検査体積の上界面の流束
			 flux_w,              // u[0][j]の検査体積の左界面の流束（flux_e[-1][j]が使えないので，特別に定義）

       ae[ni+2][nj+2], aw[ni+2][nj+2],  // 離散化方程式の係数
       an[ni+2][nj+2], as[ni+2][nj+2],
       ap[ni+2][nj+2], b[ni+2][nj+2],

			 dx, dy,      // 格子サイズ
       gr,          // グラスホッフ数
       visc,        // 動粘性係数
       a,           // 熱拡散率

       dvmax,       // 速度vの変化量の最大値
       pcnew, err,
       smax,smin,
			 umean,

       ue,vn,       // セル界面の速度（右界面，上界面）
       time, dt,      // 時刻、時間きざみ
			 umax,

       dudt, dvdt, dTdt,

       dummy
       ;

int scheme;       // 1: 中心法
                  // 2: 一次風上
                  // 3: ハイブリッド

int n,            // 時刻ステップ
    ni1, ni2, nj1, nj2, i, j, iter,
    is_u_solve[ni+2][nj+2], is_v_solve[ni+2][nj+2], is_p_solve[ni+2][nj+2];

// 以下はFullDataSave で保存せず。
double endtime = 120;            // 計算終了時刻
int nend = 10000;                // 計算終了ステップ


// ファイルポインタ
FILE *fp  = _popen("C:/gnuplot/bin/gnuplot", "w");    // gnuplotとの接続のためのファイルポインタ
FILE *fp1 = _popen("C:/gnuplot/bin/gnuplot", "w");
FILE *fp2 = _popen("C:/gnuplot/bin/gnuplot", "w");
FILE *fp3 = _popen("C:/gnuplot/bin/gnuplot", "w");

// 関数の型宣言
void sor_pc();  // 圧力補正のポアソン式をSOR法で解くルーチン
void PlotVector(), PlotStreamline(), PlotPresure(), PlotTemperature(), SaveData();  // 結果の表示ルーチン等
void FullDataSave(), FullDataLoad();
void CalFlux(double u[][nj+2], double visc, int scheme_e, int scheme_n);

//===========================
// メインプログラム
//===========================
int main() {

// 中心法
//  scheme = 1;
//  cn = 0.2; //Ra=1000 OK, Ra=10000 定常まで計算できるが，局所的な温度のアンダーシュートが残る（-0.144）
	//cn = 0.3; //Ra=1000 OK  , Ra=10000 定常まで計算できるが，局所的な温度のアンダーシュートが残る（-0.145）
	//cn = 0.4; //Ra=1000 OK  , Ra=10000 定常まで計算できるが，局所的な温度のアンダーシュートが残る（-0.145）
	//cn = 0.5; //Ra=1000 OK  , Ra=10000 出口付近で振動止まらず
  //cn = 0.6; //Ra=1000 OK  , Ra=10000 発散

// 風上
//  scheme = 2;
//  cn = 0.5;

// ハイブリッド
  scheme = 3;
  cn = 0.5;

//*********************************************
// フルデータロード（以前の計算結果を読み込んでグラフィック表示，または継続計算する場合）
//---------------------------------------------
  //FullDataLoad(); printf("フルデータを読み込みました \n Enter キーを押して下さい \n"); getchar();
//---------------------------------------------


  //***** gnuplot と接続 ほか *****

  fprintf(fp , "set terminal wxt title '速度ベクトル' size 1000,300 \n");
  fprintf(fp1, "set terminal wxt title '圧力' size 1000,300 \n");
  fprintf(fp2, "set terminal wxt title '流線' size 1000,300 \n");
  fprintf(fp3, "set terminal wxt title '温度' size 1000,300 \n");

/*
  // 大き目（本に掲載する図を取得時の設定）（クリップボードにコピーする時はwxtウィンドウを最大化すること）
  fprintf(fp , "set terminal wxt title '速度ベクトル' size 1200,400 \n");
  fprintf(fp1, "set terminal wxt title '圧力' size 1200,400 \n");
  fprintf(fp2, "set terminal wxt title '流線' size 1200,400 \n");
  fprintf(fp3, "set terminal wxt title '温度' size 1200,400 \n");
*/


  //***** 定数 *****
  gr = ra/pr;
  visc = 1./sqrt(gr);
  a = 1./sqrt(gr)/pr;
  dx = Lx/(double)ni ; dy = Ly/(double)nj ;

  ni1 = (int)(Lx1/dx + 1e-6);
  ni2 = (int)((Lx1+Lb)/dx + 1e-6);
  nj1 = (int)((Ly/2-Lb/2)/dy + 1e-6);
  nj2 = (int)((Ly/2+Lb/2)/dy + 1e-6);

  printf("ni1=%d  ni2=%d  nj1=%d  nj2=%d \n", ni1,ni2,nj1,nj2);

  //***** 計算点 *****
  // 速度u
  for(i=0; i<=ni; i++){ for(j=1; j<=nj; j++){
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
    if(i==1) {ap[i][j] = ap[i][j] + aw[i][j] ; aw[i][j] = 0;}  // 入口圧力はディリクレ境界（pc=0）
    if(i==ni){ap[i][j] = ap[i][j] + ae[i][j] ; ae[i][j] = 0;}  // 出口圧力はディリクレ境界（pc=0）
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


//*********************************************
// 以前の計算結果をフルデータロード後の描画
//---------------------------------------------
/*
  PlotStreamline();
  PlotTemperature();
  PlotVector();
  PlotPresure();
  printf("Enter キーを押して下さい \n"); getchar();
*/
//---------------------------------------------

  //**************************************************
  // 時間前進
  //**************************************************

do {
  n = n+1;
	dt = cn/(umax/dx + MAX(visc,a)*2*(1/dx/dx + 1/dy/dy));
  time = time + dt;

  if(n==1) {
    printf("この計算は時間がかかるので，Visual C++ の場合，Releaseモードで実行して下さい（Debugモードは遅いので） \n \n");
  }

  //***** 境界条件 *****
  for(i=1; i<=ni; i++){
    u[i][0] = -u[i][1];      // 下境界 u=0
    u[i][nj+1] = -u[i][nj];  // 上境界 u=0

    T[i][0] = T[i][1];      // 下境界 dT/dy=0
    T[i][nj+1] = T[i][nj];  // 上境界 dT/dy=0
  }

  for(j=1; j<=nj; j++){
    v[0][j] = -v[1][j];         // 左境界 v=0
   p[0][j] = 2*(pin-u[0][j]*u[0][j]/2) - p[1][j];  // 左境界 p=pin
//     p[0][j] = 2*pin - p[1][j];  // 左境界 p=pin
    T[0][j] = 2*Tin - T[1][j];  // 左境界 T=Tin

    u[ni+1][j] = u[ni][j];           // 右境界 du/dx=0
    v[ni+1][j] = v[ni][j];           // 右境界 dv/dx=0
    p[ni+1][j] = 2*pout - p[ni][j];  // 右境界 p=pout
    T[ni+1][j] = T[ni][j];           // 右境界 dT/dx=0
  }

  //***** NS方程式：次の時刻の速度の仮の値 *****

  //----- unew -----

  // 流束
  for(i=0; i<=ni; i++){
  for(j=0; j<=nj; j++){
    ue = (u[i][j]+u[i+1][j])/2;
    vn = (v[i][j]+v[i+1][j])/2;
		CalFlux(u, visc, scheme, scheme);
	}}

  //角柱の上下面の処理
  for(i=ni1; i<=ni2; i++){
    j=nj1;
    flux_n[i][j] = visc/dy*2*u[i][j];
    j=nj2;
    flux_n[i][j] = -visc/dy*2*u[i][j+1];
  }

  // 次の時刻の値

	i=0;                        // u[0][j] は flux_e[-1][j] が使えないので別扱い
	for(j=1; j<=nj; j++){
		flux_w = u[i][j]*u[i][j]; // 入口で du/dx=0 に注意
    dudt = (flux_w-flux_e[i][j])/dx + (flux_n[i][j-1]-flux_n[i][j])/dy   + (p[i][j]-p[i+1][j])/dx +(T[i][j]+T[i+1][j])/2;
    unew[i][j] = u[i][j]+dudt*dt;
	}

	umax = 0;
  for(i=1; i<=ni; i++){
  for(j=1; j<=nj; j++){
  if(is_u_solve[i][j]){
    dudt = (flux_e[i-1][j]-flux_e[i][j])/dx + (flux_n[i][j-1]-flux_n[i][j])/dy  + (p[i][j]-p[i+1][j])/dx +(T[i][j]+T[i+1][j])/2;
    unew[i][j] = u[i][j]+dudt*dt;
		umax = MAX(umax, fabs(unew[i][j]));
  }}}


  //----- vnew -----

  // 流束
  for(i=0; i<=ni; i++){
  for(j=0; j<=nj-1; j++){
    ue = (u[i][j]+u[i][j+1])/2;
    vn = (v[i][j]+v[i][j+1])/2;

		if(i==0) CalFlux(v, visc,      1, scheme); // 入口境界では v=0 の固定値なので風上は使わない。すなわち flux_e[0][j] は中心法（scheme = 1）で評価する。
		else     CalFlux(v, visc, scheme, scheme);
  }}

  //角柱の左右面の処理
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
  for(i=0; i<=ni; i++){
  for(j=1; j<=nj; j++){
    if(is_u_solve[i][j]) unew[i][j] = unew[i][j] + (pc[i][j]-pc[i+1][j])/dx*dt;
    if(is_v_solve[i][j]) {
			vnew[i][j] = vnew[i][j] + (pc[i][j]-pc[i][j+1])/dy*dt;
      dvmax = MAX(dvmax, fabs(vnew[i][j]-v[i][j]));
		}
    p[i][j] = p[i][j] + pc[i][j];
  }}

  //***** 温度場 *****

	// 流束
  for(i=0; i<=ni; i++){
  for(j=0; j<=nj; j++){
    ue = u[i][j];
    vn = v[i][j];

		if(i==0) CalFlux(T, a,      1, scheme); // 入口境界では T=0 の固定値なので風上は使わない。すなわち flux_e[0][j] は中心法（scheme = 1）で評価する。
		else     CalFlux(T, a, scheme, scheme);
  }}

  //角柱の上下面の処理
  for(i=ni1+1; i<=ni2; i++){
    j=nj1;
    flux_n[i][j] = a/dy*2*(T[i][j]-Tb);
    j=nj2;
    flux_n[i][j] = a/dy*2*(Tb-T[i][j+1]);
  }

  //角柱の左右面の処理
  for(j=nj1+1; j<=nj2; j++){
    i=ni1;
    flux_e[i][j] = a/dx*2*(T[i][j]-Tb);
    i=ni2;
    flux_e[i][j] = a/dx*2*(Tb-T[i+1][j]);
  }

  // 次の時刻の値に更新

  for(i=1; i<=ni; i++){
  for(j=1; j<=nj; j++){
  if(is_p_solve[i][j]){     // 温度の計算点と圧力の計算点は同じなので，is_p_solve[i][j] で代用
    dTdt = (flux_e[i-1][j]-flux_e[i][j])/dx + (flux_n[i][j-1]-flux_n[i][j])/dy;
    T[i][j] = T[i][j]+dTdt*dt;
  }}}

  //***** 速度場を新しい値に更新 *****

  for(i=0; i<=ni; i++){
  for(j=1; j<=nj; j++){
    if(is_u_solve[i][j]) u[i][j] = unew[i][j];
    if(is_v_solve[i][j]) v[i][j] = vnew[i][j];
  }}

	umean=0;
	for(j=1; j<=nj; j++) umean += u[ni][j];
	umean /= nj;

  //***** 途中結果の表示 *****

  printf("n=%d time=%f iter=%d umax=%f umean=%f dvmax=%f\n",n,time, iter,umax, umean, dvmax);

  if(n==1) {
    PlotStreamline();
    PlotTemperature();
    PlotVector();
    PlotPresure();

    printf("\n一時停止中。\n テキスト画面，グラフィック画面の位置や大きさを調節して下さい。\n");
    printf("その後，Enter キーを押すと，500ステップ毎に絵を描き直し\n");
    printf("2000ステップ毎にフルデータを保存して一時停止します。\n");
    printf("では，画面を整えたら Enter キーを押して下さい。\n");

    getchar();
  }

  if(n % 500 == 0) {   // 計算途中の速度ベクトル図を描く（計算の進行状況を見たいとき）
    PlotStreamline();
    PlotTemperature();
    PlotVector();
    PlotPresure();

    //getchar();
  }

	if(n % 2000 == 0) {
    SaveData();
    FullDataSave();
		printf("フルデータ保存して一時停止中。 \n");
		printf("dvmax の値が0 になったら定常になったので，画面のそれぞれの絵を手作業で保存し，計算をやめてよいです。 \n");
		printf("計算を継続するには Enterキーを押してください \n");
		getchar();
	}

//}while (time<endtime);
}while (n<nend);

  //***** 計算終了後の表示 *****

  PlotStreamline();
  PlotTemperature();
  PlotVector();
  PlotPresure();
  SaveData();
  FullDataSave();

  printf("計算終了。\n");
  getchar();
}

//**********************************************************************
// 移流・拡散流束の計算
//（u,v,T について同じような流束計算を3回書くのはたいへんなので，関数にした）
//
// セル界面がディリクレ境界の場合には風上法を用いないという例外処理のため，
// flux_e と flux_n のschemeは個別に与えることとした。
//**********************************************************************

void CalFlux(double u[][nj+2], double visc, int scheme_e, int scheme_n){
	  double uc, du;

		// 注目点(i,j)の右界面の流束

		uc = (u[i+1][j] + u[i][j])/2;
		du =  u[i+1][j] - u[i][j];

    if(scheme_e == 1)      flux_e[i][j] = ue*uc - visc/dx*du;                  //中心
    else if(scheme_e == 2) flux_e[i][j] = ue*uc - (visc/dx + fabs(ue)/2)*du;   //１次風上
    else                   flux_e[i][j] = ue*uc - MAX(visc/dx, fabs(ue)/2)*du; //ハイブリッド

		// 注目点(i,j)の上界面の流束

		uc = (u[i][j+1] + u[i][j])/2;
		du =  u[i][j+1] - u[i][j];

		if(scheme_n == 1)      flux_n[i][j] = vn*uc - visc/dy*du;                  //中心
    else if(scheme_n == 2) flux_n[i][j] = vn*uc - (visc/dy + fabs(vn)/2)*du;   //１次風上
    else                   flux_n[i][j] = vn*uc - MAX(visc/dy, fabs(vn)/2)*du; //ハイブリッド
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

// 入口と出口断面の速度uと温度の分布をcsvファイルで出力する

void SaveData(){
  FILE *fp;
  char name[20];

  sprintf(name, "u_T_prof_%06d.csv",n);

  fp = fopen(name, "w");
  fprintf(fp, "%d , %f \n",n,time);
  fprintf(fp, "y , u_in , u_out , T_out \n");
  fprintf(fp, "%f , %f , %f , %f \n",0.,0.,0., T[ni][1]);
  for (j=1; j<=nj; j++) fprintf(fp, "%f , %f , %f , %f \n",dy*(j-0.5),u[0][j] ,u[ni][j] , T[ni][j]);
  fprintf(fp, "%f , %f , %f , %f \n",Ly,0.,0., T[ni][nj]);

  fclose(fp);
}



//**********************************************************************
// 結果のグラフィック表示（gnuplot）
//**********************************************************************

//======================================================================
// 速度ベクトル図（gnuplot）
//======================================================================
void PlotVector(){
  double uc, vc, uscale;
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

	if(umax != 0) uscale = umax;
	else uscale = 1;

  for(i=mabiki/2+1; i<=ni; i+=mabiki) {
    //for(j=1; j<=nj; j+=2){
    for(j=1; j<=nj; j++){
      uc = (u[i][j]+u[i-1][j])/2; //セル中心の速度
      vc = (v[i][j]+v[i][j-1])/2;
      fprintf(fp, "%f %f %f %f\n", (i-0.5)*dx, (j-0.5)*dy, uc/umax*dx*mabiki*0.6, vc/uscale*dx*mabiki*0.6);
    }
  }
  fprintf(fp, "e \n");

  //-----------------------------------------------------
  //***** 角柱を描く *****
  fprintf(fp, "plot '-' with lines lc rgb 'red' \n");
  fprintf(fp, "%f %f \n", ni1*dx,nj1*dy);
  fprintf(fp, "%f %f \n", ni1*dx,nj2*dy);
  fprintf(fp, "%f %f \n", ni2*dx,nj2*dy);
  fprintf(fp, "%f %f \n", ni2*dx,nj1*dy);
  fprintf(fp, "%f %f \n", ni1*dx,nj1*dy);
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
  fprintf(fp1, "set pm3d map interpolate %d,%d \n", MAX(100/nj,1), MAX(100/nj,1));
  fprintf(fp1, "set contour \n");  // 等高線を描く

	if(pmin != pmax){
	  fprintf(fp1, "set zrange [%f:%f]\n", pmin,pmax);
    fprintf(fp1, "set cbrange[%f:%f] \n", pmin, pmax);  // カラーバーの範囲
	}
	else{
	  fprintf(fp1, "set zrange [%f:%f]\n", pmin,1.);
    fprintf(fp1, "set cbrange[%f:%f] \n", pmin, 1.);  // カラーバーの範囲
	}


  //等高線の下限，上限，間隔
//  dd = 0.05;
//  fprintf(fp1, "set cntrparam levels incremental -2.,%f,2. \n",dd);  //等高線の下限，間隔，上限

  num_cnt = 20; //等高線の数

//  dd = (pmax-pmin)/num_cnt;   // 等高線の間隔
//  fprintf(fp1, "set cntrparam levels incremental %f,%f,%f \n",pmin,dd,pmax);  //等高線の下限，間隔，上限

  fprintf(fp1, "set cntrparam levels %d \n",num_cnt);

  //等高線の線の色を単色
  fprintf(fp1, "set style line 1 lt 1 lw 1 lc rgb '#202020' \n");
  fprintf(fp1, "set style increment user \n");
  fprintf(fp1, "unset clabel \n");

  //その他設定
  fprintf(fp1, "unset key \n");
//  fprintf(fp1, "unset tics \n");

  //ラベル
  fprintf(fp1, "set label 1 at graph 0.01,1.1 '圧力  n=%d time=%f \n", n,time);

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
	double dd;

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


  fprintf(fp2, "set pm3d map \n");
  fprintf(fp2, "set contour \n");

	if(smax !=smin){
	  fprintf(fp2, "set zrange [%f:%f]\n", smin,smax);
		fprintf(fp2, "set cntrparam levels incremental %f,%f,%f \n", smin, (smax-smin)/20,smax); //等高線の下限，間隔，上限
	}
	else{
	  fprintf(fp2, "set zrange [%f:%f]\n", -1., 1.);
		fprintf(fp2, "set cntrparam levels 10 \n");
	}


//  fprintf(fp2, "set cntrparam levels 10 \n");

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
// 温度場（gnuplot）
//======================================================================

void PlotTemperature(){
  double Tver[ni+2][nj+2];   // 描画用の計算セル右上頂点での温度
	double Tmax=1,Tmin=0;
  int i,j;

	for(i=1; i<=ni; i++){
	for(j=1; j<=nj; j++){
		Tmax = MAX(Tmax, T[i][j]);
		Tmin = MIN(Tmin, T[i][j]);
	}}

	printf("Tmax=%f  Tmin=%f \n", Tmax, Tmin);

  //***** 計算セル右上頂点での温度 *****
  for(j=1; j<=nj; j++){
    T[ni+1][j] = T[ni][j];     // 右境界
  }

  for(i=0; i<=ni+1; i++){
    T[i][0] = T[i][1];      // 下境界
    T[i][nj+1] = T[i][nj];  // 上境界
  }

  for(i=0; i<=ni; i++){
  for(j=0; j<=nj; j++){
    Tver[i][j] = (T[i][j]+T[i+1][j]+T[i][j+1]+T[i+1][j+1])/4;
  }}

  for(i=ni1; i<=ni2; i++){
    Tver[i][nj2] = Tb;
    Tver[i][nj1] = Tb;
  }

  for(j=nj1; j<=nj2; j++){
    Tver[ni1][j] = Tb;
    Tver[ni2][j] = Tb;
  }

  //***** 描画の設定 *****
  fprintf(fp3, "set multiplot \n");
  fprintf(fp3, "set colorbox \n");  //カラーバーを描く

  fprintf(fp3, "set xrange [0:%f]\n",Lx);
  fprintf(fp3, "set yrange [0:%f]\n", Ly);
  fprintf(fp3, "set size ratio %f \n", Ly/Lx);

  fprintf(fp3, "set palette defined ( 0 '#0000ff',1 '#00b8ff',2 '#00ffff',3 '#00ffb8',4 '#00ff00',5 '#b8ff00',6 '#ffff00',7 '#ffb800',8 '#ff0000') \n"); // レインボーカラー
  fprintf(fp3, "set pm3d map interpolate %d,%d \n", MAX(100/nj,1), MAX(100/nj,1));

  fprintf(fp3, "set contour \n");  // 等高線を描く

  //等高線の下限，上限，間隔
  fprintf(fp3, "set zrange [%f:%f]\n", Tmin,Tmax);   // 渦度0の等渦度線が描かれるよう工夫
  fprintf(fp3, "set cbrange[%f:%f] \n", Tmin,Tmax);  // カラーバーの範囲

  fprintf(fp3, "set cntrparam levels 10 \n");  //等高線を10本描く

  //等高線の線の色を単色
  fprintf(fp3, "set style line 1 lt 1 lw 1 lc rgb '#000000' \n");
  fprintf(fp3, "set style increment user \n");
  fprintf(fp3, "unset clabel \n");


  //その他設定
  fprintf(fp3, "unset key \n");
//  fprintf(fp3, "unset tics \n");
//  fprintf(fp3, "set nosurface \n"); //この文を実行すると色塗りがされない

  //ラベル
  fprintf(fp3, "set label 1 at graph 0.01,1.1 '温度' \n");

  //***** 描画 *****
  fprintf(fp3, "splot '-' \n");
  for(i=0; i<=ni; i++) {
    for(j=0; j<=nj; j++){
      fprintf(fp3, "%f %f %f\n", i*dx, j*dy, Tver[i][j]);
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
  fwrite( &Lb , sizeof(double), 1, fp );
  fwrite( &Lx1, sizeof(double), 1, fp );
  fwrite( &Lx , sizeof(double), 1, fp );
  fwrite( &Ly , sizeof(double), 1, fp );
  fwrite( &ra , sizeof(double), 1, fp );
  fwrite( &pr , sizeof(double), 1, fp );
  fwrite( &alpha , sizeof(double), 1, fp );
  fwrite( &converge , sizeof(double), 1, fp );
  fwrite( &scheme, sizeof(int), 1, fp );

  fwrite( &n, sizeof(int), 1, fp );
  fwrite( &time, sizeof(double), 1, fp );
  fwrite( &umax, sizeof(double), 1, fp );

  num = (ni+2)*(nj+2);
  fwrite( u, sizeof(double), num, fp );
  fwrite( v, sizeof(double), num, fp );
  fwrite( p, sizeof(double), num, fp );
  fwrite( T, sizeof(double), num, fp );

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
  fread( &Lb , sizeof(double), 1, fp );
  fread( &Lx1, sizeof(double), 1, fp );
  fread( &Lx , sizeof(double), 1, fp );
  fread( &Ly , sizeof(double), 1, fp );
  fread( &ra , sizeof(double), 1, fp );
  fread( &pr , sizeof(double), 1, fp );
  fread( &alpha , sizeof(double), 1, fp );
  fread( &converge , sizeof(double), 1, fp );
  fread( &scheme, sizeof(int), 1, fp );

  fread( &n, sizeof(int), 1, fp );
  fread( &time, sizeof(double), 1, fp );
  fread( &umax, sizeof(double), 1, fp );

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
  fread( T, sizeof(double), num, fp );

  fclose( fp );

}
