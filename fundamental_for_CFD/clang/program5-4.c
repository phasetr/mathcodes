//2次元非粘性オイラー方程式の2次精度有限体積法 : Double Mach Problem
//空間再構築：vanLeer TVD リミター
//リーマンソルバー: flux difference splitting (Roeスキーム)
//時間積分：2段Runge-Kutta法
#include <stdio.h>
#define _USE_MATH_DEFINES
#include <math.h>
#include <stdlib.h>

#define pi M_PI                 // 円周率
//#define x_data 256               // x方向の計算領域における格子セル数
//#define y_data 80               // y方向の計算領域における格子セル数
#define x_data 256               // x方向の計算領域における格子セル数
#define y_data 80               // y方向の計算領域における格子セル数
#define N_Ghost 2               // ゴーストセル数
#define i_max x_data+2*N_Ghost  // x方向の格子セル数
#define j_max y_data+2*N_Ghost  // y方向の格子セル数
#define XL 0.0                  // 計算領域左端の座標
#define XR 3.2                  // 計算領域右端の座標
#define YB 0.0                  // 計算領域下端の座標
#define YU 1.0                  // 計算領域上端の座標
#define gamma 1.4               // 比熱比（理想気体）
#define cfl 0.2                 // CFL数
#define tstop 0.2               // 計算終了時刻
FILE *fp;                       // 描画ソフトgnuplotの実行ファイルのポインタ

int i,j;                        // セル番号
double x[i_max + 1][j_max + 1]; // セル境界のx座標
double y[i_max + 1][j_max + 1]; // セル境界のy座標
double dx;                      // x方向の格子間隔（等間隔とする）
double dy;                      // y方向の格子間隔（等間隔とする）
double dt;                      // 時間刻み
int n;                          // 時間ステップ
double t;                       // 計算時間
double lambda_max;              // 最大特性速度

double rho[i_max][j_max];       // 密度“rho”のセル平均値
double rhou[i_max][j_max];      // 運動量“rho・u”のセル平均値
double rhov[i_max][j_max];      // 運動量“rho・v”のセル平均値
double rhoE[i_max][j_max];      // 総エネルギー“rho・E”のセル平均値

double rho_b[i_max][j_max];     // 密度の1step前の値
double rhou_b[i_max][j_max];    // 運動量の1step前の値
double rhov_b[i_max][j_max];    // 運動量の1step前の値
double rhoE_b[i_max][j_max];    // 総エネルギーの1step前の値

double u[i_max][j_max];         // 速度“u”のセル平均値
double v[i_max][j_max];         // 速度“v”のセル平均値
double p[i_max][j_max];         // 圧力“p”のセル平均値

	/* >>>> 空間再構築から求めたセル境界左・右側の保存変数 >>>> */
double rho_L[i_max + 1][j_max + 1];    // 左側の密度“rho^L”
double rho_R[i_max + 1][j_max + 1];    // 右側の密度“rho^R”
double rho_B[i_max + 1][j_max + 1];    // 下側の密度“rho^B”
double rho_U[i_max + 1][j_max + 1];    // 上側の密度“rho^U”
double rhou_L[i_max + 1][j_max + 1];   // 左側の運動量“(rho・u)^L”
double rhou_R[i_max + 1][j_max + 1];   // 右側の運動量“(rho・u)^R”
double rhou_B[i_max + 1][j_max + 1];   // 下側の運動量“(rho・u)^B”
double rhou_U[i_max + 1][j_max + 1];   // 上側の運動量“(rho・u)^U”
double rhov_L[i_max + 1][j_max + 1];   // 左側の運動量“(rho・v)^L”
double rhov_R[i_max + 1][j_max + 1];   // 右側の運動量“(rho・v)^R”
double rhov_B[i_max + 1][j_max + 1];   // 下側の運動量“(rho・v)^B”
double rhov_U[i_max + 1][j_max + 1];   // 上側の運動量“(rho・v)^U”
double rhoE_L[i_max + 1][j_max + 1];   // 左側の総エネルギー“(rho・E)^L”
double rhoE_R[i_max + 1][j_max + 1];   // 右側の総エネルギー“(rho・E)^R”
double rhoE_B[i_max + 1][j_max + 1];   // 下側の総エネルギー“(rho・E)^B”
double rhoE_U[i_max + 1][j_max + 1];   // 上側の総エネルギー“(rho・E)^U”

	/* >>>> セル境界における数値流束 >>>> */
double f1[i_max + 1][j_max + 1];    // x方向の密度流束
double f2[i_max + 1][j_max + 1];    // x方向の運動量流束
double f3[i_max + 1][j_max + 1];    // x方向の運動量流束
double f4[i_max + 1][j_max + 1];    // x方向の全エネルギー流束

double g1[i_max + 1][j_max + 1];    // y方向の密度流束
double g2[i_max + 1][j_max + 1];    // y方向の運動量流束
double g3[i_max + 1][j_max + 1];    // y方向の運動量流束
double g4[i_max + 1][j_max + 1];    // y方向の全エネルギー流束

double Lh_rho[i_max][j_max];        // 密度の空間離散式
double Lh_rhou[i_max][j_max];       // 運動量の空間離散式
double Lh_rhov[i_max][j_max];       // 運動量の空間離散式
double Lh_rhoE[i_max][j_max];       // 総エネルギーの空間離散式

double umax,vmax;                   // x,y方向の最大速度

		/* >>>> 関数プロトタイプ宣言 >>>> */
double max(double x, double y);     // 実数 x と y の最大値関数
double min(double x, double y);     // 実数 x と y の最小値関数
void initc();                       // 初期条件設定関数
void boundary_condition();          // 境界条件設定関数
void reconstruction_x();            // x方向の空間再構築
void reconstruction_y();            // y方向の空間再構築
double sign(double);                // 変数の符号を求める関数
double vanLeer(double);             // vanLeerリミター
void riemann_roe_x();               // x方向リーマンソルバー関数（flux difference splitting）
void riemann_roe_y();               // y方向リーマンソルバー関数（flux difference splitting）
void RK_1st();                      // 時間発展/時間積分関数
void RK_2nd();                      // 時間発展/時間積分関数
void cfl_check();                   // CFL条件関数
void graph();                       // 結果可視化関数
void graph1();                       // 可視化出力関数

int main()	//main関数  
{
	fp = _popen("C:/gnuplot/bin/gnuplot", "w");  //gnuplotを動かすパイプを作成
	fprintf(fp, "set terminal windows title 'Solution plots 2D' size 1300,400 \n"); // 出力先，タイトル，サイズ指定	
	initc();                                      // 初期条件設定
	n = 0;                                        // 時間ステップカウンター初期値
	t = 0.0;                                      // 初期時刻
	while (t < tstop)                             // 時間積分ループ
	{
	/* Runge-Kutta法の1段目 */
		boundary_condition();
		/* 空間再構築を行い,セル境界値を求める */
		reconstruction_x();                        // x方向の空間再構築
		reconstruction_y();                        // y方向の空間再構築
		/* リーマンソルバー選択し,数値流速を求める */ 
		riemann_roe_x();
		riemann_roe_y();
		cfl_check();	                           // 最大特性速度に基づくCFL条件から
		                                           // 時間積分刻みを決める
		RK_1st();	                           // RK 1段目
		
	/* Runge-Kutta法の2段目 */
		boundary_condition();
		/* 空間再構築を行い,セル境界値を求める */
		reconstruction_x();                        // x方向の空間再構築
		reconstruction_y();                        // y方向の空間再構築
		/* リーマンソルバー選択し,数値流速を求める */ 
		riemann_roe_x();
		riemann_roe_y();
		cfl_check();	                           // 最大特性速度に基づくCFL条件から
		                                           // 時間積分刻みを決める
		RK_2nd();	                           // RK 2段目
		
		n = n + 1;
		t = t + dt;
		if(n%50==0) printf("n=%d, t=%f, \n", n, t); // 時間積分回数と時間を表示
		if (n % 50 == 0)graph(); 
	}
	graph();
	graph1();
	printf("Press Enter key to finish.\n");
	getchar();
}


void initc() 
{
	double xc,yc,yy;
	dx = (XR - XL) / (double)x_data;
	dy = (YU - YB) / (double)y_data;
	for (j = 0; j <= j_max; j++) {
		x[0][j] = XL - (N_Ghost * dx);
	}
	for (i = 0; i <= i_max; i++) {
		y[i][0] = YB - (N_Ghost * dy);
	}
	for (i = 1; i <= i_max; i++) {
		for (j = 1; j <= j_max; j++) {
			x[i][j] = x[i - 1][j] + dx;       // 区間の長さ(隣接のデータ点間の距離)
			y[i][j] = y[i][j - 1] + dy;
		}
	}
	/* >>>>各変数(セル平均)の初期値を０にする>>>> */
	for (i = 0; i <= i_max - 1; i++) {
		for (j = 0; j <= j_max - 1; j++) {
			rho[i][j]  = 0.0;
			rhou[i][j] = 0.0;
			rhov[i][j] = 0.0;
			rhoE[i][j] = 0.0;
			u[i][j]    = 0.0;
			v[i][j]    = 0.0;
			p[i][j]    = 0.0;
		}
	}
	/* >>>> 各変数(境界再構築値)及び数値流束の初期値を0にする >>>> */
	for (i = 0; i <= i_max; i++) {
		for (j = 0; j <= j_max; j++) {
			rho_L[i][j]  = 0.0;
			rho_R[i][j]  = 0.0;
			rhou_L[i][j] = 0.0;
			rhou_R[i][j] = 0.0;
			rhov_L[i][j] = 0.0;
			rhov_R[i][j] = 0.0;
			rhoE_L[i][j] = 0.0;
			rhoE_R[i][j] = 0.0;
			rho_B[i][j]  = 0.0;
			rho_U[i][j]  = 0.0;
			rhou_B[i][j] = 0.0;
			rhou_U[i][j] = 0.0;
			rhov_B[i][j] = 0.0;
			rhov_U[i][j] = 0.0;
			rhoE_B[i][j] = 0.0;
			rhoE_U[i][j] = 0.0;
			f1[i][j] = 0.0;
			f2[i][j] = 0.0;
			f3[i][j] = 0.0;
			f4[i][j] = 0.0;
			g1[i][j] = 0.0;
			g2[i][j] = 0.0;
			g3[i][j] = 0.0;
			g4[i][j] = 0.0;
		}
	}
	/* >>>> 各変数の初期値を与える >>>> */
	for (i = 0; i < i_max; i++) {
		for (j = 0; j < j_max; j++) {
			xc = (x[i][j]+x[i+1][j])/2.0;
			yc = (y[i][j]+y[i][j+1])/2.0;
			yy = sqrt(3.0)*(xc-1.0/6.0);
			if(yc < yy){
				rho[i][j] = 1.4;
				u[i][j]   = 0.0;
				v[i][j]   = 0.0;
				p[i][j]   = 1.0;
			}else{
				rho[i][j] = 8.0;
				u[i][j]   = 8.25*cos(pi/6.0);
				v[i][j]   = -8.25*sin(pi/6.0);
				p[i][j]   = 116.5;
			}
			rhou[i][j] = rho[i][j] * u[i][j];
			rhov[i][j] = rho[i][j] * v[i][j];
			rhoE[i][j] = p[i][j] / (gamma - 1.0) + rho[i][j] * (u[i][j]*u[i][j]+v[i][j]*v[i][j])/2.0;
		}
	}

}

void boundary_condition()
{
	double xc;
	double shock_speed = 11.54; // 衝撃波の速度
	
//left boundary
	for(i=0;i<N_Ghost;i++){
		for(j=0;j<j_max;j++){
			 rho[i][j] =  rho[N_Ghost][j];
			rhou[i][j] = rhou[N_Ghost][j];
			rhov[i][j] = rhov[N_Ghost][j];
			rhoE[i][j] = rhoE[N_Ghost][j];
		}
	}
	
//right boundary
	for(i=x_data+N_Ghost;i<i_max;i++){
		for(j=0;j<j_max;j++){
			 rho[i][j] =  rho[x_data+N_Ghost-1][j];
			rhou[i][j] = rhou[x_data+N_Ghost-1][j];
			rhov[i][j] = rhov[x_data+N_Ghost-1][j];
			rhoE[i][j] = rhoE[x_data+N_Ghost-1][j];
		}
	}
	
//up boundary
	for(j=y_data+N_Ghost;j<j_max;j++){
		for(i=0;i<i_max;i++){
			xc = (x[i][j]+x[i+1][j])/2.0;
			if(xc<1.0/6.0+1.0/sqrt(3.0)+t*shock_speed){
				rho[i][j] = 8.0;
				  u[i][j] = 8.25*cos(pi/6.0);
				  v[i][j] = -8.25*sin(pi/6.0);
				  p[i][j] = 116.5;
			}else{
				rho[i][j] = 1.4;
				  u[i][j] = 0.0;
				  v[i][j] = 0.0;
				  p[i][j] = 1.0;
			}
			rhou[i][j] = rho[i][j] * u[i][j];
			rhov[i][j] = rho[i][j] * v[i][j];
			rhoE[i][j] = p[i][j] / (gamma - 1.0) + rho[i][j] * (u[i][j]*u[i][j]+v[i][j]*v[i][j])/2.0;
		}
	}
	
//bottom boudary
	for(j=0;j<N_Ghost;j++){
		for(i=0;i<i_max;i++){
			xc = (x[i][j]+x[i+1][j])/2.0;
			if(xc<1.0/6.0){
				 rho[i][j] = 1.4;
				   u[i][j] = 0.0;
				   v[i][j] = 0.0;
				   p[i][j] = 1.0;
				rhou[i][j] = rho[i][j] * u[i][j];
				rhov[i][j] = rho[i][j] * v[i][j];
				rhoE[i][j] = p[i][j] / (gamma - 1.0) + rho[i][j] * (u[i][j]*u[i][j]+v[i][j]*v[i][j])/2.0;
			}else{
				 rho[i][j] =   rho[i][(N_Ghost-1-j)+N_Ghost];
				rhou[i][j] =  rhou[i][(N_Ghost-1-j)+N_Ghost];
				rhov[i][j] = -rhov[i][(N_Ghost-1-j)+N_Ghost];
				rhoE[i][j] =  rhoE[i][(N_Ghost-1-j)+N_Ghost];
			}
		}
	}
}


void reconstruction_x()
{
	double rL,rR,phiL,phiR;
	
	/* 密度 */
	for (i = N_Ghost-1; i <= x_data+N_Ghost-1; i++) {
		for (j = N_Ghost; j <= y_data+N_Ghost; j++) {
			rL = (rho[i][j] - rho[i-1][j])
				/ (rho[i+1][j] - rho[i][j] + sign(rho[i+1][j] - rho[i][j])*1.0e-5);
			rR = (rho[i+2][j] - rho[i+1][j])
				/ (rho[i+1][j] - rho[i][j] + sign(rho[i+1][j] - rho[i][j])*1.0e-5);
			phiL  = vanLeer(rL);
			phiR  = vanLeer(rR);
			rho_L[i+1][j] = rho[i][j]   + 0.5*phiL*(rho[i+1][j] - rho[i][j]);
			rho_R[i+1][j] = rho[i+1][j] - 0.5*phiR*(rho[i+1][j] - rho[i][j]);
		}
	}
	
	/* 運動量 rhou */
	for (i = N_Ghost-1; i <= x_data+N_Ghost-1; i++) {
		for (j = N_Ghost; j <= y_data+N_Ghost; j++) {
			rL = (rhou[i][j] - rhou[i-1][j])
				/ (rhou[i+1][j] - rhou[i][j] + sign(rhou[i+1][j] - rhou[i][j])*1.0e-5);
			rR = (rhou[i+2][j] - rhou[i+1][j])                 
				/ (rhou[i+1][j] - rhou[i][j] + sign(rhou[i+1][j] - rhou[i][j])*1.0e-5);
			phiL  = vanLeer(rL);
			phiR  = vanLeer(rR);
			rhou_L[i+1][j] = rhou[i][j]   + 0.5*phiL*(rhou[i+1][j] - rhou[i][j]);
			rhou_R[i+1][j] = rhou[i+1][j] - 0.5*phiR*(rhou[i+1][j] - rhou[i][j]);
		}
	}
	
	/* 運動量 rhov */
	for (i = N_Ghost-1; i <= x_data+N_Ghost-1; i++) {
		for (j = N_Ghost; j <= y_data+N_Ghost; j++) {
			rL = (rhov[i][j] - rhov[i-1][j])
				/ (rhov[i+1][j] - rhov[i][j] + sign(rhov[i+1][j] - rhov[i][j])*1.0e-5);
			rR = (rhov[i+2][j] - rhov[i+1][j])
				/ (rhov[i+1][j] - rhov[i][j] + sign(rhov[i+1][j] - rhov[i][j])*1.0e-5);
			phiL  = vanLeer(rL);
			phiR  = vanLeer(rR);
			rhov_L[i+1][j] = rhov[i][j]   + 0.5*phiL*(rhov[i+1][j] - rhov[i][j]);
			rhov_R[i+1][j] = rhov[i+1][j] - 0.5*phiR*(rhov[i+1][j] - rhov[i][j]);
		}
	}
	
	/* 総エネルギー */
	for (i = N_Ghost-1; i <= x_data+N_Ghost-1; i++) {
		for (j = N_Ghost; j <= y_data+N_Ghost; j++) {
			rL = (rhoE[i][j] - rhoE[i-1][j])
				/ (rhoE[i+1][j] - rhoE[i][j] + sign(rhoE[i+1][j] - rhoE[i][j])*1.0e-5);
			rR = (rhoE[i+2][j] - rhoE[i+1][j])
				/ (rhoE[i+1][j] - rhoE[i][j] + sign(rhoE[i+1][j] - rhoE[i][j])*1.0e-5);
			phiL  = vanLeer(rL);
			phiR  = vanLeer(rR);
			rhoE_L[i+1][j] = rhoE[i][j]   + 0.5*phiL*(rhoE[i+1][j] - rhoE[i][j]);
			rhoE_R[i+1][j] = rhoE[i+1][j] - 0.5*phiR*(rhoE[i+1][j] - rhoE[i][j]);
		}
	}
}

void reconstruction_y()
{
	double rL,rR,phiL,phiR;
	
	/* 密度 */
	for (j = N_Ghost-1; j <= y_data+N_Ghost-1; j++) {
		for (i = N_Ghost; i <= x_data+N_Ghost; i++) {
			rL = (rho[i][j] - rho[i][j-1]) 
				/ (rho[i][j+1] - rho[i][j] + sign(rho[i][j+1] - rho[i][j])*1.0e-5);
			rR = (rho[i][j+2] - rho[i][j+1]) 
				/ (rho[i][j+1] - rho[i][j] + sign(rho[i][j+1] - rho[i][j])*1.0e-5);
			phiL  = vanLeer(rL);
			phiR  = vanLeer(rR);
			rho_B[i][j+1] = rho[i][j]   + 0.5*phiL*(rho[i][j+1] - rho[i][j]);
			rho_U[i][j+1] = rho[i][j+1] - 0.5*phiR*(rho[i][j+1] - rho[i][j]);
		}
	}
	
	/* 運動量 rhou */
	for (j = N_Ghost-1; j <= y_data+N_Ghost-1; j++) {
		for (i = N_Ghost; i <= x_data+N_Ghost; i++) {
			rL = (rhou[i][j]-rhou[i][j-1])                         // セル境界(i+1/2)の左側の勾配比
				/ (rhou[i][j+1] - rhou[i][j] + sign(rhou[i][j+1] - rhou[i][j])*1.0e-5);
			rR = (rhou[i][j+2] - rhou[i][j+1])                   // セル境界(i+1/2)の右側の勾配比
				/ (rhou[i][j+1] - rhou[i][j] + sign(rhou[i][j+1] - rhou[i][j])*1.0e-5);
			phiL  = vanLeer(rL);
			phiR  = vanLeer(rR);
			rhou_B[i][j+1] = rhou[i][j]   + 0.5*phiL*(rhou[i][j+1] - rhou[i][j]);
			rhou_U[i][j+1] = rhou[i][j+1] - 0.5*phiR*(rhou[i][j+1] - rhou[i][j]);
		}
	}
	
	/* 運動量 rhov */
	for (j = N_Ghost-1; j <= y_data+N_Ghost-1; j++) {
		for (i = N_Ghost; i <= x_data+N_Ghost; i++) {
			rL = (rhov[i][j] - rhov[i][j-1]) 
				/ (rhov[i][j+1] - rhov[i][j] + sign(rhov[i][j+1] - rhov[i][j])*1.0e-5);
			rR = (rhov[i][j+2] - rhov[i][j+1]) 
				/ (rhov[i][j+1] - rhov[i][j] + sign(rhov[i][j+1] - rhov[i][j])*1.0e-5);
			phiL  = vanLeer(rL);
			phiR  = vanLeer(rR);
			rhov_B[i][j+1] = rhov[i][j]   + 0.5*phiL*(rhov[i][j+1] - rhov[i][j]);
			rhov_U[i][j+1] = rhov[i][j+1] - 0.5*phiR*(rhov[i][j+1] - rhov[i][j]);
		}
	}
	
	/* 総エネルギー */
	for (j = N_Ghost-1; j <= y_data+N_Ghost-1; j++) {
		for (i = N_Ghost; i <= x_data+N_Ghost; i++) {
			rL = (rhoE[i][j]-rhoE[i][j-1]) 
				/ (rhoE[i][j+1] - rhoE[i][j] + sign(rhoE[i][j+1] - rhoE[i][j])*1.0e-5);
			rR = (rhoE[i][j+2] - rhoE[i][j+1]) 
				/ (rhoE[i][j+1] - rhoE[i][j] + sign(rhoE[i][j+1] - rhoE[i][j])*1.0e-5);
			phiL  = vanLeer(rL);
			phiR  = vanLeer(rR);
			rhoE_B[i][j+1] = rhoE[i][j]   + 0.5*phiL*(rhoE[i][j+1] - rhoE[i][j]);
			rhoE_U[i][j+1] = rhoE[i][j+1] - 0.5*phiR*(rhoE[i][j+1] - rhoE[i][j]);
		}
	}
}

double sign(double x){                             // 変数の符号を求める関数
	if (x >= 0) return 1;
	else return -1;
}

double vanLeer(double x){                          // van Leer limiter
	return (x + fabs(x)) / (1 + fabs(x));
}

void riemann_roe_x()
{
	double epsilon = 0.15;
	double r_L, r_R, rE_L, rE_R, u_L, u_R, v_L, v_R, p_L, p_R, h_L, h_R, q, u2;
	double lambda1, lambda2, lambda3, lambda4;
	double r_avg, u_avg, v_avg, h_avg, c_avg;
	double rsqrt_L, rsqrt_R;
	double dw1, dw2, dw3, dw4;
	
	for (i = N_Ghost; i <= x_data+N_Ghost; i++) {
		for (j = N_Ghost; j <= y_data+N_Ghost; j++) {
			/*>>>>** 保存変数からプリミティブ変数を求める ***/
			r_L = rho_L[i][j];
			r_R = rho_R[i][j];
			rE_L = rhoE_L[i][j];
			rE_R = rhoE_R[i][j];
			u_L = rhou_L[i][j] / rho_L[i][j];
			u_R = rhou_R[i][j] / rho_R[i][j];
			v_L = rhov_L[i][j] / rho_L[i][j];
			v_R = rhov_R[i][j] / rho_R[i][j];
			p_L = (gamma - 1.0) * (rhoE_L[i][j] - (u_L*u_L + v_L*v_L)*rho_L[i][j]/2.0);
			p_R = (gamma - 1.0) * (rhoE_R[i][j] - (u_R*u_R + v_R*v_R)*rho_R[i][j]/2.0);
			h_L = (rhoE_L[i][j] + p_L) / rho_L[i][j];
			h_R = (rhoE_R[i][j] + p_R) / rho_R[i][j];
			
			/*>>>>** Roe平均を計算する ***/
			rsqrt_L = sqrt(r_L);
			rsqrt_R = sqrt(r_R);
			r_avg = rsqrt_L*rsqrt_R;
			u_avg = (u_L*rsqrt_L + u_R*rsqrt_R) / (rsqrt_L + rsqrt_R);
			v_avg = (v_L*rsqrt_L + v_R*rsqrt_R) / (rsqrt_L + rsqrt_R);
			h_avg = (h_L*rsqrt_L + h_R*rsqrt_R) / (rsqrt_L + rsqrt_R);
			c_avg = sqrt((gamma - 1)*(h_avg - (u_avg*u_avg+v_avg*v_avg) / 2.0));
			q     = (u_avg*u_avg+v_avg*v_avg)/2.0;
			
			/*>>>>** Jacobian行列 A の固有値（特性速度）の平均を計算する ***/
			lambda1 = u_avg;
			lambda2 = u_avg;
			lambda3 = u_avg + c_avg;
			lambda4 = u_avg - c_avg;
			
			/*>>>> エントロピー修正 (Harten) ***/
			lambda1 = fabs(lambda1)>2.0*epsilon ?
				fabs(lambda1) : lambda1*lambda1 / (4.*epsilon) + epsilon;
			lambda2 = fabs(lambda2)>2.0*epsilon ?
				fabs(lambda2) : lambda2*lambda2 / (4.*epsilon) + epsilon;
			lambda3 = fabs(lambda3)>2.0*epsilon ?
				fabs(lambda3) : lambda3*lambda3 / (4.*epsilon) + epsilon;
			lambda4 = fabs(lambda4) > 2.0*epsilon ?
				fabs(lambda4) : lambda4*lambda4 / (4.*epsilon) + epsilon;
			
			/* >>>> セル境界における左右の特性変数の差を計算する >>>> */
			dw1 = (r_R - r_L) - (p_R - p_L) / (c_avg*c_avg);
			dw2 = -(v_R - v_L);
			dw3 = (u_R - u_L) + (p_R - p_L) / (r_avg*c_avg);
			dw4 = (u_R - u_L) - (p_R - p_L) / (r_avg*c_avg);
			
			/* >>>> 数値流束を計算する >>>> */
			f1[i][j] = 0.5*(r_L*u_L + r_R*u_R) - 0.5*(
						  lambda1*dw1
						+ lambda3*dw3*r_avg/(2.0*c_avg)
						- lambda4*dw4*r_avg/(2.0*c_avg) );
			
			f2[i][j] = 0.5*( ((gamma-1.0)*rE_R+0.5*((3.0-gamma)*r_R*u_R*u_R-(gamma-1.0)*r_R*v_R*v_R)) + ((gamma-1.0)*rE_L+0.5*((3.0-gamma)*r_L*u_L*u_L-(gamma-1.0)*r_L*v_L*v_L)) ) - 0.5*(
						  lambda1*dw1*u_avg
						+ lambda3*dw3*r_avg/(2.0*c_avg)*(u_avg+c_avg)
						- lambda4*dw4*r_avg/(2.0*c_avg)*(u_avg-c_avg) );
			
			f3[i][j] = 0.5*(r_R*u_R*v_R + r_L*u_L*v_L) - 0.5*(
						  lambda1*dw1*v_avg
						- lambda2*dw2*r_avg
						+ lambda3*dw3*r_avg/(2.0*c_avg)*v_avg
						- lambda4*dw4*r_avg/(2.0*c_avg)*v_avg );
			
			f4[i][j] = 0.5*( (gamma*rE_R*u_R - (gamma-1.0)*0.5*r_R*u_R*(u_R*u_R+v_R*v_R)) + (gamma*rE_L*u_L - (gamma-1.0)*0.5*r_L*u_L*(u_L*u_L+v_L*v_L)) ) - 0.5*(
						  lambda1*dw1*q
						- lambda2*dw2*r_avg*v_avg
						+ lambda3*dw3*r_avg/(2.0*c_avg)*(q+c_avg*c_avg/(gamma-1.0)+c_avg*u_avg)
						- lambda4*dw4*r_avg/(2.0*c_avg)*(q+c_avg*c_avg/(gamma-1.0)-c_avg*u_avg) );
			
			/* >>>> 最大速度を計算する >>>> */
			if(i == N_Ghost && j == N_Ghost){
				umax = max(lambda3,lambda4);
			}else{
				u2 = max(lambda3,lambda4);
				if(u2 >= umax) umax = u2;
			}
		}
	}
}

void riemann_roe_y()
{
	double epsilon = 0.15;
	double r_B, r_U, rE_B, rE_U, u_B, u_U, v_B, v_U, p_B, p_U, h_B, h_U, q, v2;
	double lambda1, lambda2, lambda3, lambda4;
	double r_avg, u_avg, v_avg, h_avg, c_avg;
	double rsqrt_B, rsqrt_U;
	double dw1, dw2, dw3, dw4;
	
	for (i = N_Ghost; i <= x_data+N_Ghost; i++) {
		for (j = N_Ghost; j <= y_data+N_Ghost; j++) {
			/*>>>>** 保存変数からプリミティブ変数を求める ***/
			r_B = rho_B[i][j];
			r_U = rho_U[i][j];
			rE_B = rhoE_B[i][j];
			rE_U = rhoE_U[i][j];
			u_B = rhou_B[i][j] / rho_B[i][j];
			u_U = rhou_U[i][j] / rho_U[i][j];
			v_B = rhov_B[i][j] / rho_B[i][j];
			v_U = rhov_U[i][j] / rho_U[i][j];
			p_B = (gamma - 1.0)*(rhoE_B[i][j] - (u_B*u_B + v_B*v_B)*rho_B[i][j]/2.0);
			p_U = (gamma - 1.0)*(rhoE_U[i][j] - (u_U*u_U + v_U*v_U)*rho_U[i][j]/2.0);
			h_B = (rhoE_B[i][j] + p_B) / rho_B[i][j];
			h_U = (rhoE_U[i][j] + p_U) / rho_U[i][j];

			/*>>>>** Roe平均を計算する ***/
			rsqrt_B = sqrt(r_B);
			rsqrt_U = sqrt(r_U);
			r_avg = rsqrt_B*rsqrt_U;
			u_avg = (u_B*rsqrt_B + u_U*rsqrt_U) / (rsqrt_B + rsqrt_U);
			v_avg = (v_B*rsqrt_B + v_U*rsqrt_U) / (rsqrt_B + rsqrt_U);
			h_avg = (h_B*rsqrt_B + h_U*rsqrt_U) / (rsqrt_B + rsqrt_U);
			c_avg = sqrt((gamma - 1)*(h_avg - (u_avg*u_avg+v_avg*v_avg) / 2.0));
			q     = (u_avg*u_avg+v_avg*v_avg)/2.0;

			/*>>>>** Jacobian行列 A の固有値（特性速度）の平均を計算する ***/
			lambda1 = v_avg;
			lambda2 = v_avg;
			lambda3 = v_avg + c_avg;
			lambda4 = v_avg - c_avg;

			/*>>>> エントロピー修正 (Harten) ***/
			lambda1 = fabs(lambda1)>2.0*epsilon ?
				fabs(lambda1) : lambda1*lambda1 / (4.*epsilon) + epsilon;
			lambda2 = fabs(lambda2)>2.0*epsilon ?
				fabs(lambda2) : lambda2*lambda2 / (4.*epsilon) + epsilon;
			lambda3 = fabs(lambda3)>2.0*epsilon ?
				fabs(lambda3) : lambda3*lambda3 / (4.*epsilon) + epsilon;
			lambda4 = fabs(lambda4)>2.0*epsilon ?
				fabs(lambda4) : lambda4*lambda4 / (4.*epsilon) + epsilon;
			
			/* >>>> セル境界における左右の特性変数の差を計算する >>>> */
			dw1 = (r_U - r_B) - (p_U - p_B) / (c_avg*c_avg);
			dw2 = (u_U - u_B);
			dw3 = (v_U - v_B) + (p_U - p_B) / (r_avg*c_avg);
			dw4 = (v_U - v_B) - (p_U - p_B) / (r_avg*c_avg);
			
			/* >>>> 数値流束を計算する >>>> */
			g1[i][j] = 0.5*(r_B*v_B + r_U*v_U) - 0.5*(
						  lambda1*dw1
						+ lambda3*dw3*r_avg/(2.0*c_avg)
						- lambda4*dw4*r_avg/(2.0*c_avg) );
			
			g2[i][j] = 0.5*(r_B*u_B*v_B + r_U*u_U*v_U) - 0.5*(
						  lambda1*dw1*u_avg
						+ lambda2*dw2*r_avg
						+ lambda3*dw3*u_avg*r_avg/(2.0*c_avg)
						- lambda4*dw4*u_avg*r_avg/(2.0*c_avg) );
			
			g3[i][j] = 0.5*( ((gamma-1.0)*rE_B+0.5*((3.0-gamma)*r_B*v_B*v_B-(gamma-1.0)*r_B*u_B*u_B)) + ((gamma-1.0)*rE_U+0.5*((3.0-gamma)*r_U*v_U*v_U-(gamma-1.0)*r_U*u_U*u_U)) ) - 0.5*(
						  lambda1*dw1*v_avg
						+ lambda3*dw3*r_avg/(2.0*c_avg)*(v_avg+c_avg)
						- lambda4*dw4*r_avg/(2.0*c_avg)*(v_avg-c_avg) );
			
			g4[i][j] = 0.5*( (gamma*rE_B*v_B - (gamma-1.0)*0.5*r_B*v_B*(u_B*u_B+v_B*v_B)) + (gamma*rE_U*v_U - (gamma-1.0)*0.5*r_U*v_U*(u_U*u_U+v_U*v_U)) ) - 0.5*(
						  lambda1*dw1*q
						+ lambda2*dw2*r_avg*u_avg
						+ lambda3*dw3*r_avg/(2.0*c_avg)*(q+c_avg*c_avg/(gamma-1.0)+c_avg*v_avg)
						- lambda4*dw4*r_avg/(2.0*c_avg)*(q+c_avg*c_avg/(gamma-1.0)-c_avg*v_avg) );
			
			/* >>>> 最大速度を計算する >>>> */
			if(i == N_Ghost && j == N_Ghost){
				vmax = max(lambda3,lambda4);
			}else{
				v2 = max(lambda3,lambda4);
				if(v2 >= vmax) vmax = v2;
			}
		}
	}
}


void RK_1st()                                      // 2次Runge-Kutta法の1段目
{
	for (i = N_Ghost; i <= x_data+N_Ghost-1; i++) {
		for (j = N_Ghost; j <= y_data+N_Ghost-1; j++) {
			Lh_rho[i][j]  = (f1[i][j]-f1[i+1][j])/dx + (g1[i][j]-g1[i][j+1])/dy;
			Lh_rhou[i][j] = (f2[i][j]-f2[i+1][j])/dx + (g2[i][j]-g2[i][j+1])/dy;
			Lh_rhov[i][j] = (f3[i][j]-f3[i+1][j])/dx + (g3[i][j]-g3[i][j+1])/dy;
			Lh_rhoE[i][j] = (f4[i][j]-f4[i+1][j])/dx + (g4[i][j]-g4[i][j+1])/dy;
			rho_b[i][j]   = rho[i][j];
			rhou_b[i][j]  = rhou[i][j];
			rhov_b[i][j]  = rhov[i][j];
			rhoE_b[i][j]  = rhoE[i][j];
			rho[i][j]     = rho_b[i][j]  + dt * Lh_rho[i][j];
			rhou[i][j]    = rhou_b[i][j] + dt * Lh_rhou[i][j];
			rhov[i][j]    = rhov_b[i][j] + dt * Lh_rhov[i][j];
			rhoE[i][j]    = rhoE_b[i][j] + dt * Lh_rhoE[i][j];
		}
	}
}

void RK_2nd()                                      // 2次Runge-Kutta法の2段目
{
	for (i = N_Ghost; i <= x_data+N_Ghost-1; i++) {
		for (j = N_Ghost; j <= y_data+N_Ghost-1; j++) {
			 rho[i][j] =  rho_b[i][j] + 0.5 * (  Lh_rho[i][j] + (f1[i][j]-f1[i+1][j])/dx + (g1[i][j]-g1[i][j+1])/dy ) * dt;
			rhou[i][j] = rhou_b[i][j] + 0.5 * ( Lh_rhou[i][j] + (f2[i][j]-f2[i+1][j])/dx + (g2[i][j]-g2[i][j+1])/dy ) * dt;
			rhov[i][j] = rhov_b[i][j] + 0.5 * ( Lh_rhov[i][j] + (f3[i][j]-f3[i+1][j])/dx + (g3[i][j]-g3[i][j+1])/dy ) * dt;
			rhoE[i][j] = rhoE_b[i][j] + 0.5 * ( Lh_rhoE[i][j] + (f4[i][j]-f4[i+1][j])/dx + (g4[i][j]-g4[i][j+1])/dy ) * dt;
		}
	}
}

void cfl_check() 
{
	double vel;

	vel = max(umax,vmax);             
	if(vel == umax) dt = cfl*dx/vel;          // 最大速度からdtを計算
	else            dt = cfl*dy/vel;
	
	if((t+(dt) > tstop) && (t != tstop)){
      		dt = tstop - t;
	}
}

double max(double x, double y)                     // 2つの実数x,yの最大値を計算する
{
	double z;
	z = x > y ? x : y;
	return (z);

}

double min(double x, double y)                     // 2つの実数x,yの最小値を計算する
{
	double z;
	z = x < y ? x : y;
	return(z);
}

void graph0() // 計算結果の可視化ツール
{
	fprintf(fp, "set xrange [0.0:3.2]\n");
	fprintf(fp, "set yrange [0.0:1.0]\n");
	fprintf(fp, "set zrange [1.8:22.0]\n");
	fprintf(fp, "set xlabel \"x\"\n");
	fprintf(fp, "set ylabel \"y\"\n");
	fprintf(fp, "set zlabel \"z\"\n");
	fprintf(fp, "unset key\n");
	fprintf(fp, "set pm3d map\n");
	fprintf(fp, "set palette rgbformulae 22,13,-31\n");
	fprintf(fp, "splot '-' w pm3d\n");
	
	for (j = N_Ghost; j < j_max - N_Ghost; j++) {
		for (i = N_Ghost; i < i_max - N_Ghost; i++) {
			fprintf(fp,"%lf %lf %lf\n",(x[i][j]+x[i+1][j])/2,(y[i][j]+y[i][j+1])/2,rho[i][j]);
		}
		fprintf(fp,"\n");
	}
	fprintf(fp, "e \n");
	fflush(fp);
}


void graph() // 計算結果の可視化ツール
{
	fprintf(fp, "set xrange [0.0:3.2]\n");
	fprintf(fp, "set yrange [0.0:0.8]\n");
//	fprintf(fp, "set zrange [1.8:22.0]\n");
	fprintf(fp, "set ytics 0,0.4,0.8 \n");
	fprintf(fp, "set xtics  0.0, 0.5,3.2 \n");
	fprintf(fp, "unset ztics \n");

	//*** その他の指定 ***
	fprintf(fp, "unset key \n");		// 凡例を書かない
	fprintf(fp, "set ticslevel 0\n");	// Z軸の原点をXY平面と一致させる
//	fprintf(fp, "set hidden3d \n");		// 陰線消去
	fprintf(fp, "set contour\n");		// 等高線も描く
	fprintf(fp, "set cntrparam cubicspline \n");
//	fprintf(fp, "set contour base \n");		// 
	fprintf(fp, "set nosurface \n");		//
	fprintf(fp, "set view 0,0 \n");		// 
	fprintf(fp, "set cntrparam levels 20 \n");	// 等高線の数を20本にする
	//*** データを送る ***
	fprintf(fp, "splot '-' with lines  lt 1 lw 1  \n");	// これから送るデータを用いて３次元グラフを描く
	
	for (j = 1; j < j_max - N_Ghost; j++) {
		for (i = 1; i < i_max - N_Ghost; i++) {
			fprintf(fp, "%lf %lf %lf\n", (x[i][j] + x[i + 1][j]) / 2, (y[i][j] + y[i][j + 1]) / 2, rho[i][j]);
		}
		fprintf(fp, "\n");
	}
	fprintf(fp, "e \n");
	fflush(fp);
}


void graph1() // 計算結果の可視化ツール
{
	fprintf(fp, "set terminal postscript eps \n");
	//可視化の出力先指定（epsファイルに出力）
	fprintf(fp, "set output 'output.eps \n");
	fprintf(fp, "set xrange [0.0:3.2]\n");
	fprintf(fp, "set yrange [0.0:0.8]\n");
	fprintf(fp, "set ytics 0,0.4,0.8 \n");
	fprintf(fp, "set xtics 0.0, 0.5,3.2 \n");
	fprintf(fp, "unset ztics \n");
	//*** その他の指定 ***
	fprintf(fp, "unset key \n");		// 凡例を書かない
	fprintf(fp, "set ticslevel 0\n");	// Z軸の原点をXY平面と一致させる
	fprintf(fp, "set contour\n");		// 等高線も描く
	fprintf(fp, "set nosurface \n");		//
	fprintf(fp, "set view 0,0 \n");		// 
	fprintf(fp, "set cntrparam levels 20 \n");	// 等高線の数を20本にする
	//*** データを送る ***
	fprintf(fp, "splot '-' with lines lw 1.0  lc rgb 'black',  \n");	// これから送るデータを用いて２次元グラフを描く
		for (j = 1; j < j_max - N_Ghost; j++) {
		for (i = 1; i < i_max - N_Ghost; i++) {
			fprintf(fp, "%lf %lf %lf\n", (x[i][j] + x[i + 1][j]) / 2, (y[i][j] + y[i][j + 1]) / 2, rho[i][j]);
		}
		fprintf(fp, "\n");
	}
	fprintf(fp, "e \n");
	fflush(fp);
}
