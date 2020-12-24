//一次元オイラー方程式のTVD法
//空間再構築：minmod,vanLeer, vanAlbada,superbee リミター
//時間積分：2段Runge-Kutta法
#include <stdio.h>
#include <math.h>
#include <stdlib.h>

#define i_max 100           // 格子セル数
#define XL -1.0             // 計算領域左端の座標
#define XR 1.0              // 計算領域右端の座標
#define gamma 1.4           // 比熱比（理想気体）
#define cfl 0.4             // CFL数
#define tstop 0.48          // 計算終了時刻
FILE *fp;                   // 描画ソフトgnuplotの実行ファイルのポインタ

/*
		 i=0       i=1                i=i_max-2 i=i_max-1 　　　-> セル番号
	 |---------|---------|---  -  ---|---------|---------|
	i=0       i=1                          i=i_max-1  i=i_max　 -> 境界番号
		i番目セルの左境界(x_{i-1/2})の番号はi、
		右境界(x_{i+1/2})のの番号はi+1とする)
															  */


int i;                      // セル番号
double x[i_max + 1];        // セル境界の座標
double dx;                  // 格子間隔（等間隔とする）
double dt;                  // 時間刻み
int n;                      // 時間ステップ
double t;                   // 計算時間
int riemann_option;         // リーマンソルバー選択オプション
                            // riemann_option=1: flux vector splitting
                            // riemann_option=2: flux difference splitting
double lambda_max;          // 最大特性速度

double rho[i_max];          // 密度“rho”のセル平均値
double rhou[i_max];         // 運動量“rho・u”のセル平均値
double rhoE[i_max];         // 総エネルギー“rho・E”のセル平均値

double rho_b[i_max];        // 密度の1step前の値
double rhou_b[i_max];       // 運動量の1step前の値
double rhoE_b[i_max];       // 総エネルギーの1step前の値

double rho_star[i_max];     // 密度のRK1段目
double rhou_star[i_max];    // 運動量のRK1段目
double rhoE_star[i_max];    // 総エネルギーのRK1段目

double u[i_max];            // 速度“u”のセル平均値
double p[i_max];            // 圧力“p”のセル平均値

	/* >>>> 空間再構築から求めたセル境界左・右側の保存変数 >>>> */
double rho_L[i_max + 1];    // 左側の密度“rho^L”
double rho_R[i_max + 1];    // 右側の密度“rho^R”
double rhou_L[i_max + 1];   // 左側の運動量“(rho・u)^L”
double rhou_R[i_max + 1];   // 右側の運動量“(rho・u)^R”
double rhoE_L[i_max + 1];   // 左側の総エネルギー“(rho・E)^L”
double rhoE_R[i_max + 1];   // 右側の総エネルギー“(rho・E)^R”

	/* >>>> セル境界における数値流束 >>>> */
double f1[i_max + 1];       // 密度流束
double f2[i_max + 1];       // 運動量流束
double f3[i_max + 1];       // 全エネルギー流束

double Lh_rho[i_max];       // 密度の空間離散式
double Lh_rhou[i_max];      // 運動量の空間離散式
double Lh_rhoE[i_max];      // 総エネルギーの空間離散式

double rhoe[i_max], ue[i_max], pe[i_max];          //各格子上における密度,速度,圧力の厳密解


		/* >>>> 関数プロトタイプ宣言 >>>> */
double max(double x, double y);       // 実数 x と y の最大値関数
double min(double x, double y);       // 実数 x と y の最小値関数
void initc();                         // 初期条件設定関数
void reconstruction(int);             // 空間再構築
double sign(double);                  // 変数の符号を求める関数
double minmod(double);                // minmodリミター
double vanLeer(double);               // vanLeerリミター
double vanAlbada(double);             // vanAlbadaリミター
double superbee(double);              // superbeeリミター
void riemann_sw();                    // リーマンソルバー関数（flux vector splitting）
void riemann_roe();                   // リーマンソルバー関数（flux difference splitting）
void RK_1st();                        // 時間発展/時間積分関数
void RK_2nd();                        // 時間発展/時間積分関数
void bc();							  // 境界条件
void cfl_check();                     // CFL条件関数
void exact_solution();                // 厳密解関数
void graph();                         // 結果可視化関数
void graph1();                        // 結果可視化関数（output.epsへ出力）

int main()	//main関数  
{
	int sw1;
	fp = _popen("C:/gnuplot/bin/gnuplot", "w"); //描画ツールgnuplotのファイルポインタ指定
	fprintf(fp, "set terminal windows title 'Solution plots' size 800,600 \n");//可視化の出力先指定（PCの画面に表示）
	initc();                                       // 初期条件設定
	n = 0;                                         // 時間ステップカウンター初期値
	t = 0.0;                                       // 初期時刻
	printf("再構築法を選ぶ\n");
	printf("minmodリミター                  : 1 \n");
	printf("vanLeerリミター                 : 2 \n");
	printf("vanAlbadaリミター               : 3 \n");
	printf("superbeeリミター                : 4 \n");
	scanf_s("%d", &sw1);
	printf("リーマンソルバーを選ぶ\n");
	printf("flux vector splitting    ： 1 \n");
	printf("flux difference splitting： 2 \n");
	scanf_s("%d", &riemann_option);
	
	while (t <= tstop)                             // 時間積分ループ
	{
		n = n + 1;
		t = t + dt;
		cfl_check();	                           // 最大特性速度に基づくCFL条件から
		                                           // 時間積分刻みを決める
	/* Runge-Kutta法の1段目 */
		/* 空間再構築を行い,セル境界値を求める */
		reconstruction(sw1);                       // 空間再構築
		/* リーマンソルバー選択し,数値流束を求める */
		switch (riemann_option) {
		case 1:                                    // flux vector splitting
			riemann_sw();
			break;
		case 2:                                    // flux difference splitting
			riemann_roe();
			break;
		default:
			printf("当該オプションがありません。\n");
			break;
		}
		RK_1st();	                               // RK 1段目
		bc();							  // 境界条件
		
	/* Runge-Kutta法の2段目 */
		/* 空間再構築を行い,セル境界値を求める */
		reconstruction(sw1);                       // 空間再構築
		/* リーマンソルバー選択し,数値流束を求める */
		switch (riemann_option) {   
		case 1:                                    // flux vector splitting
			riemann_sw();
			break;
		case 2:                                    // flux difference splitting
			riemann_roe();
			break;
		default:
			printf("当該オプションがありません。\n");
			break;
		}
		RK_2nd();	                               // RK 2段目
		bc();							  // 境界条件
		exact_solution();                          // 厳密解を計算する
		graph();                                   // 計算結果グラフで表示する
		printf("n=%d, t=%f, \n", n, t);            // 時間積分回数と時間を表示
	}
	//*****************************************
	graph1();	// 計算結果のグラフを保存する
	//*****************************************
	printf("Press Enter key to finish.\n");
	getchar();
	getchar();
}


void initc() //一次元衝撃波管（ショックチューブ）問題の初期値
{
	dx = (XR - XL) / ((double)i_max - 4.0);  //　格子間隔
	x[0] = XL - 2.0 * dx;
	for (i = 1; i <= i_max; i++) {
		x[i] = x[i - 1] + dx;      // セル境界の座標
	}
	/* >>>>各変数(セル平均)の初期値を０にする>>>> */
	for (i = 0; i <= i_max - 1; i++) {
		rho[i] = 0.0;
		rhou[i] = 0.0;
		rhoE[i] = 0.0;
		u[i] = 0.0;
		p[i] = 0.0;
	}
	/* >>>> 各変数(境界再構築値)及び数値流束の初期値を0にする >>>> */
	for (i = 0; i <= i_max; i++) {
		rho_L[i] = 0.0;
		rho_R[i] = 0.0;
		rhou_L[i] = 0.0;
		rhou_R[i] = 0.0;
		rhoE_L[i] = 0.0;
		rhoE_R[i] = 0.0;

		f1[i] = 0.0;
		f2[i] = 0.0;
		f3[i] = 0.0;
	}
	/* >>>> 各変数の初期値を与える（Sodのテスト問題）>>>> */
	for (i = 0; i <= i_max - 1; i++) {
		if (x[i] <= 0.0) {
			rho[i] = 1.0;  
			u[i] = 0.0;
			p[i] = 1.0;
			rhou[i] = rho[i] * u[i];
			rhoE[i] = 0.5*rhou[i] * u[i] + p[i] / (gamma - 1.0);
		}
		else {
			rho[i] = 0.125;
			u[i] = 0.0;
			p[i] = 0.1;
			rhou[i] = rho[i] * u[i];
			rhoE[i] = 0.5*rhou[i] * u[i] + p[i] / (gamma - 1.0);
		}
	}
}

void reconstruction(int sw1)
{
	double rL,rR,phiL,phiR;
	
	/* 密度 */
	for (i = 1; i <= i_max-3; i++) {
		rL = (rho[i]-rho[i-1])                         // セル境界(i+1/2)の左側の勾配比
			/ (rho[i+1] - rho[i] + sign(rho[i+1] - rho[i])*1.0e-5);
		rR = (rho[i+2] - rho[i+1])                   // セル境界(i+1/2)の右側の勾配比
			/ (rho[i+1] - rho[i] + sign(rho[i+1] - rho[i])*1.0e-5);
		switch (sw1)
		{                                          // リミターを計算する
		case 1:                                    // Minmod limiter
			phiL = minmod(rL);
			phiR = minmod(rR);
			break;
		case 2:                                    // van Leer limiter
			phiL =vanLeer(rL);
			phiR =vanLeer(rR);
			break;
		case 3:                                    // van Albada limiter
			phiL = vanAlbada(rL);
			phiR = vanAlbada(rR);
			break;
		case 4:                                    // Superbee limiter
			phiL = superbee(rL);
			phiR = superbee(rR);
			break;
		default:
			printf("空間再構築法を正しく選択されていません。");
			break;
		}
		rho_L[i+1]  = rho[i]    + 0.5*phiL*(rho[i+1]  - rho[i]);
		rho_R[i+1]  = rho[i+1]  - 0.5*phiR*(rho[i+1]  - rho[i]);
	}
	/* 運動量 */
	for (i = 1; i <= i_max-3; i++) {
		rL = (rhou[i]-rhou[i-1])                         // セル境界(i+1/2)の左側の勾配比
			/ (rhou[i+1] - rhou[i] + sign(rhou[i+1] - rhou[i])*1.0e-5);
		rR = (rhou[i+2] - rhou[i+1])                   // セル境界(i+1/2)の右側の勾配比
			/ (rhou[i+1] - rhou[i] + sign(rhou[i+1] - rhou[i])*1.0e-5);
		switch (sw1)
		{                                          // リミターを計算する
		case 1:                                    // Minmod limiter
			phiL = minmod(rL);
			phiR = minmod(rR);
			break;
		case 2:                                    // van Leer limiter
			phiL = vanLeer(rL);
			phiR = vanLeer(rR);
			break;
		case 3:                                    // van Albada limiter
			phiL = vanAlbada(rL);
			phiR = vanAlbada(rR);
			break;
		case 4:                                    // Superbee limiter
			phiL = superbee(rL);
			phiR = superbee(rR);
			break;
		default:
			printf("空間再構築法を正しく選択されていません。");
			break;
		}
		rhou_L[i+1]  = rhou[i]    + 0.5*phiL*(rhou[i+1]  - rhou[i]);
		rhou_R[i+1]  = rhou[i+1]  - 0.5*phiR*(rhou[i+1]  - rhou[i]);
	}
	/* 総エネルギー */
	for (i = 1; i <= i_max-3; i++) {
		rL = (rhoE[i]-rhoE[i-1])                         // セル境界(i+1/2)の左側の勾配比
			/ (rhoE[i+1] - rhoE[i] + sign(rhoE[i+1] - rhoE[i])*1.0e-5);
		rR = (rhoE[i+2] - rhoE[i+1])                   // セル境界(i+1/2)の右側の勾配比
			/ (rhoE[i+1] - rhoE[i] + sign(rhoE[i+1] - rhoE[i])*1.0e-5);
		switch (sw1)
		{                                          // リミターを計算する
		case 1:                                    // Minmod limiter
			phiL = minmod(rL);
			phiR = minmod(rR);
			break;
		case 2:                                    // van Leer limiter
			phiL = vanLeer(rL);
			phiR = vanLeer(rR);
			break;
		case 3:                                    // van Albada limiter
			phiL = vanAlbada(rL);
			phiR = vanAlbada(rR);
			break;
		case 4:                                    // Superbee limiter
			phiL = superbee(rL);
			phiR = superbee(rR);
			break;
		default:
			printf("空間再構築法を正しく選択されていません。");
			break;
		}
		rhoE_L[i+1]  = rhoE[i]    + 0.5*phiL*(rhoE[i+1]  - rhoE[i]);
		rhoE_R[i+1]  = rhoE[i+1]  - 0.5*phiR*(rhoE[i+1]  - rhoE[i]);
	}
}

double sign(double x){                             // 変数の符号を求める関数
	if (x >= 0) return 1;
	else return -1;
}

double minmod(double x){                           // Minmod limiter
	return fmax(0.0,fmin(1.0,x));
}

double vanLeer(double x){                          // van Leer limiter
	return (x + fabs(x)) / (1 + fabs(x));
}

double vanAlbada(double x){                        // van Albada limiter
	return (x + x*x) / (1 + x*x);
}

double superbee(double x){                         // Superbee limiter
	return fmax(fmax(0.0,fmin(1.0,2.0*x)),fmin(2.0,x));
}

void riemann_sw()
// Stagger-Warming のリーマンソルバー（flux vector splitting 法）
{
	double r_L, r_R, u_L, u_R, p_L, p_R, h_L, h_R, c_L, c_R;
	double lambda1_p, lambda2_p, lambda3_p, lambda1_n, lambda2_n, lambda3_n;
	double f1_p, f2_p, f3_p, f1_n, f2_n, f3_n;
	
	for (i = 2; i <= i_max - 2; i++) {
		
		/*>>>>** 保存変数からプリミティブ変数を求める ***/
		r_L = rho_L[i];
		r_R = rho_R[i];
		u_L = rhou_L[i] / rho_L[i];
		u_R = rhou_R[i] / rho_R[i];
		p_L = (gamma - 1.)*(rhoE_L[i] - .5*rhou_L[i] * rhou_L[i] / rho_L[i]);
		p_R = (gamma - 1.)*(rhoE_R[i] - .5*rhou_R[i] * rhou_R[i] / rho_R[i]);
		h_L = (rhoE_L[i] + p_L) / rho_L[i];
		h_R = (rhoE_R[i] + p_R) / rho_R[i];
		c_L = sqrt(gamma*p_L / rho_L[i]);
		c_R = sqrt(gamma*p_R / rho_R[i]);
		
		/*>>>>** 正と負の特性速度を求める ***/
		lambda1_p = max(0.0, u_L);
		lambda2_p = max(0.0, u_L + c_L);
		lambda3_p = max(0.0, u_L - c_L);
		lambda1_n = min(0.0, u_R);
		lambda2_n = min(0.0, u_R + c_R);
		lambda3_n = min(0.0, u_R - c_R);
		
		/*>>>>** 正と負の数値流束を求める ***/
		f1_p = (gamma - 1.) / gamma*r_L*lambda1_p
			+ r_L / (2.*gamma)*lambda2_p
			+ r_L / (2.*gamma)*lambda3_p;
		f2_p = (gamma - 1.) / gamma*r_L*lambda1_p*u_L
			+ r_L / (2.*gamma)*lambda2_p*(u_L + c_L)
			+ r_L / (2.*gamma)*lambda3_p*(u_L - c_L);
		f3_p = (gamma - 1.) / gamma*r_L*lambda1_p*u_L*u_L / 2.0
			+ r_L / (2.*gamma)*lambda2_p
			*(u_L*u_L / 2.0 + c_L*c_L / (gamma - 1.) + c_L*u_L)
			+ r_L / (2.*gamma)*lambda3_p
			*(u_L*u_L / 2.0 + c_L*c_L / (gamma - 1.) - c_L*u_L);
		
		f1_n = (gamma - 1.) / gamma*r_R*lambda1_n
			+ r_R / (2.*gamma)*lambda2_n
			+ r_R / (2.*gamma)*lambda3_n;
		f2_n = (gamma - 1.) / gamma*r_R*lambda1_n*u_R
			+ r_R / (2.*gamma)*lambda2_n*(u_R + c_R)
			+ r_R / (2.*gamma)*lambda3_n*(u_R - c_R);
		f3_n = (gamma - 1.) / gamma*r_R*lambda1_n*u_R*u_R / 2.0
			+ r_R / (2.*gamma)*lambda2_n
			*(u_R*u_R / 2.0 + c_R*c_R / (gamma - 1.) + c_R*u_R)
			+ r_R / (2.*gamma)*lambda3_n
			*(u_R*u_R / 2.0 + c_R*c_R / (gamma - 1.) - c_R*u_R);
		
		/*>>>>** 数値流束を計算する ***/
		f1[i] = f1_p + f1_n;
		f2[i] = f2_p + f2_n;
		f3[i] = f3_p + f3_n;
	}

}

void riemann_roe()
// Roeのリーマンソルバー（flux difference splitting 法）
{
	double epsilon = 0.15;
	double r_L, r_R, rE_L, rE_R, u_L, u_R, p_L, p_R, h_L, h_R, c_L, c_R;
	double lambda1, lambda2, lambda3;
	double r_avg, u_avg, h_avg, c_avg;
	double rsqrt_L, rsqrt_R;
	double dw1, dw2, dw3;
	
	for (i = 2; i <= i_max - 2; i++) {
		/*>>>>** 保存変数からプリミティブ変数を求める ***/
		r_L = rho_L[i];
		r_R = rho_R[i];
		rE_L = rhoE_L[i];
		rE_R = rhoE_R[i];
		u_L = rhou_L[i] / rho_L[i];
		u_R = rhou_R[i] / rho_R[i];
		p_L = (gamma - 1.)*(rhoE_L[i] - .5*rhou_L[i] * rhou_L[i] / rho_L[i]);
		p_R = (gamma - 1.)*(rhoE_R[i] - .5*rhou_R[i] * rhou_R[i] / rho_R[i]);
		h_L = (rhoE_L[i] + p_L) / rho_L[i];
		h_R = (rhoE_R[i] + p_R) / rho_R[i];
		c_L = sqrt(gamma*p_L / rho_L[i]);
		c_R = sqrt(gamma*p_R / rho_R[i]);

		/*>>>>** Roe平均を計算する ***/
		rsqrt_L = sqrt(r_L);
		rsqrt_R = sqrt(r_R);
		r_avg = rsqrt_L*rsqrt_R;
		u_avg = (u_L*rsqrt_L + u_R*rsqrt_R) / (rsqrt_L + rsqrt_R);
		h_avg = (h_L*rsqrt_L + h_R*rsqrt_R) / (rsqrt_L + rsqrt_R);
		c_avg = sqrt((gamma - 1)*(h_avg - u_avg*u_avg / 2.0));

		/*>>>>** Jacobian行列 A の固有値（特性速度）の平均を計算する ***/
		lambda1 = u_avg;
		lambda2 = u_avg + c_avg;
		lambda3 = u_avg - c_avg;

		/*>>>> エントロピー修正 (Harten) ***/
		lambda1 = fabs(lambda1)>2.0*epsilon ?
			fabs(lambda1) : lambda1*lambda1 / (4.*epsilon) + epsilon;
		lambda2 = fabs(lambda2)>2.0*epsilon ?
			fabs(lambda2) : lambda2*lambda2 / (4.*epsilon) + epsilon;
		lambda3 = fabs(lambda3)>2.0*epsilon ?
			fabs(lambda3) : lambda3*lambda3 / (4.*epsilon) + epsilon;

		/* >>>> セル境界における左右の特性変数の差を計算する >>>> */
		dw1 = r_R - r_L - (p_R - p_L) / (c_avg*c_avg);
		dw2 = u_R - u_L + (p_R - p_L) / (r_avg*c_avg);
		dw3 = u_R - u_L - (p_R - p_L) / (r_avg*c_avg);


		/* >>>> 数値流束を計算する >>>> */
		f1[i] = 0.5*(r_L*u_L + r_R*u_R)
			- 0.5*(lambda1*dw1
				+ lambda2*r_avg / (2.*c_avg)*dw2
				- lambda3*r_avg / (2.*c_avg)*dw3);

		f2[i] = 0.5*(r_L*u_L*u_L + p_L + r_R*u_R*u_R + p_R)
			- 0.5*(lambda1*dw1*u_avg
				+ lambda2*r_avg / (2.*c_avg)*dw2*(u_avg + c_avg)
				- lambda3*r_avg / (2.*c_avg)*dw3*(u_avg - c_avg));

		f3[i] = 0.5*(rE_L*u_L + p_L*u_L + rE_R*u_R + p_R*u_R)
			- 0.5*(lambda1*dw1*u_avg*u_avg / 2.0
				+ lambda2*r_avg / (2.*c_avg)*dw2*(h_avg + c_avg*u_avg)
				- lambda3*r_avg / (2.*c_avg)*dw3*(h_avg - c_avg*u_avg));
	}
}

void RK_1st()                                      // 2次Runge-Kutta法の1段目
{
	for (i = 2; i <= i_max - 3; i++) {
		Lh_rho[i]  = (f1[i] - f1[i + 1])/ (x[i + 1] - x[i]);
		Lh_rhou[i] = (f2[i] - f2[i + 1])/ (x[i + 1] - x[i]);
		Lh_rhoE[i] = (f3[i] - f3[i + 1])/ (x[i + 1] - x[i]);
		rho_star[i]  = rho[i]  + dt * Lh_rho[i];
		rhou_star[i] = rhou[i] + dt * Lh_rhou[i];
		rhoE_star[i] = rhoE[i] + dt * Lh_rhoE[i];
		rho_b[i]  = rho[i];
		rhou_b[i] = rhou[i];
		rhoE_b[i] = rhoE[i];
		rho[i]    = rho_star[i];
		rhou[i]   = rhou_star[i];
		rhoE[i]   = rhoE_star[i];
	}
}

void RK_2nd()                                      // 2次Runge-Kutta法の2段目
{
	for (i = 2; i <= i_max - 3; i++) {
		rho[i]  = rho_b[i]  + 0.5 * ( Lh_rho[i]  + (f1[i] - f1[i + 1]) / (x[i + 1] - x[i]) ) * dt;
		rhou[i] = rhou_b[i] + 0.5 * ( Lh_rhou[i] + (f2[i] - f2[i + 1]) / (x[i + 1] - x[i]) ) * dt;
		rhoE[i] = rhoE_b[i] + 0.5 * ( Lh_rhoE[i] + (f3[i] - f3[i + 1]) / (x[i + 1] - x[i]) ) * dt;
	}
}

void bc() {    //境界条件
	 //　計算領域左端の境界条件
	rho[1] = rho[2];
	rhou[1] = rhou[2];
	rhoE[1] = rhoE[2];
	rho[0] = rho[1];
	rhou[0] = rhou[1];
	rhoE[0] = rhoE[1];

	//　計算領域右端の境界条件
	rho[i_max - 2] = rho[i_max - 3];
	rhou[i_max - 2] = rhou[i_max - 3];
	rhoE[i_max - 2] = rhoE[i_max - 3];
	rho[i_max - 1] = rho[i_max - 2];
	rhou[i_max - 1] = rhou[i_max - 2];
	rhoE[i_max - 1] = rhoE[i_max - 2];
}

void cfl_check()                                   // CFL条件関数
{
	double p, u, c,dt_min;
	lambda_max = 0.0;
	for (i = 0; i <= i_max-1; i++) {
		u = rhou[i] / rho[i];                      // 流速
		p = (gamma - 1.)*(rhoE[i] - .5*rhou[i] * rhou[i] / rho[i]); // 圧力
		c = sqrt(gamma*p / rho[i]);                // 音速
		lambda_max = fabs(lambda_max) > fabs(u+c) ? lambda_max : u+c;
		lambda_max = fabs(lambda_max) > fabs(u-c) ? lambda_max : u-c;
		dt_min = cfl*dx / max(fabs(lambda_max), 0.1);
	}
	dt = dt_min;
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

double F(double p21, double p1, double p5, double rho1, double rho5, 
	     double u1, double u5)
{
	double c1, c5, w;
	c1 = sqrt(gamma * p1 / rho1);
	c5 = sqrt(gamma * p5 / rho5);
	w = p5 * pow(1.0 + (double)(gamma - 1)
		* (u5 - u1 - c1 / (double)gamma * (double)(p21 - 1.0)
		* pow((double)((gamma + 1.0) / gamma * (p21 - 1.0)) / 2.0 
		+ 1.0, -1.0 / 2.0))
		/ c5 / 2.0, (double)(2 * gamma / (gamma - 1))) / p1 - (double)p21;
	return (w);
}

void exact_solution()   // 厳密リーマンソルバー
{
	int it, itmax;
	double df, fm, fmm, pm, pmm;
	double rho1, rho2, rho3, rho5;
	double p1, p2, p3, p5;
	double u1, u2, u3, u5;
	double c1, c3, c4, c5;
	double eps, error;
	double x0, xs, xc, xrt, xrh;
	double p21, Vc, Vs,Vrt,Vrh;

	x0 = 0.0;                                      // 初期区切りの位置
	
	// 初期値から領域１の変数値を与える
	rho1 = 0.125;                                  // 密度
	p1 = 0.1;                                      // 圧力
	u1 = 0.0;                                      // 流速
	c1 = sqrt(gamma * p1 / rho1); //音速
	
	// 初期値から領域５の変数値を与える
	rho5 = 1.0;
	p5 = 1.0;
	u5 = 0.0;
	c5 = sqrt(gamma * p5 / rho5);
	
	// Secant反復法により、衝撃波前後の圧力ジャンプを求める
	p21 = p1 / p5;
	pm = 21+0.01;
	pmm = pm+0.01;
	fmm = F(pm, p1, p5, rho1, rho5, u1, u5);
	itmax = 20;                                    // 反復回数の上限
	it = 0;
	eps = 1.e-5;                                   // 収束判定条件
	
	do {
		it++;
		fm = F(p21, p1, p5, rho1, rho5, u1, u5);
		df = fm - fmm;
		p21 = p21 - (p21 - pmm) * fm / (df + 1.0e-8*df / (fabs(df) + 1.0e-8));
		error = fabs(p21 - pm) / pm;
		pmm = pm;
		pm = p21;
		fmm = fm;
//		printf("反復回数=%d,p21=%f\n",it, p21);
	} while (error > eps && it<=itmax);
	if (it>itmax)
	{
//		printf("反復最大回数を超えた。\n");
	}
	
	//	領域2の物理量を計算する
	rho2 = rho1*(p21 + (gamma - 1) / (gamma + 1)) /
		   ((gamma - 1)*p21 / (gamma + 1) + 1);       // 密度
	u2 = u1 + c1*sqrt(2.0/gamma)*(p21 - 1) /
		sqrt(gamma - 1+p21 * (gamma + 1) );           // 流速
	p2 = p21*p1;                                      // 圧力
//	printf("rho2=%f,u2=%f,p2=%f\n",rho2,u2,p2);
	
	//	領域3の物理量を計算する
	u3 = u2;
	p3 = p2;
	rho3 = rho5*pow(p3 / p5, 1.0/gamma);
	c3 = sqrt(gamma * p3 / rho3);
//	printf("rho3=%f,u3=%f,p3=%f\n", rho3, u3, p3);
	
	//	各波の速度を計算する
	Vs = u1 +c1*sqrt((gamma+1)/(2.*gamma)*(p21-1)+1); // 衝撃波
	Vc = u3;                                          // 接触不連続
	Vrt = u3 - c3;                                    // 膨張波末端の速度
	Vrh = u5-c5;                                      // 膨張波先端の速度
//	printf("Vs=%f,Vc=%f,Vrt=%f,,Vrh=%f\n", Vs, Vc,Vrt,Vrh);
	
	// t時刻における波の位置を計算する
	xs = x0 + Vs * t;                                 // 衝撃波
	xc = x0 + Vc * t;                                 // 接触不連続
	xrt = x0 + Vrt * t;                               // 膨張波末端の速度
	xrh = x0 + Vrh * t;                               // 膨張波先端の速度
//	printf("xsh=%f,xcd=%f,xft=%f,xhd=%f\n", xs, xc, xrt,xrh);
	
	// 計算格子に解を与える
	for (i = 0; i <= i_max-1; i++) {
		// 領域５
		if (x[i] < xrh) {
			rhoe[i] = rho5;
			pe[i] = p5;
			ue[i] = u5;
		}
		
		// 領域４
		else if (x[i] <= xrt) {
			ue[i] = 2. / (gamma + 1) * (0.5*(gamma-1)*u5+c5 + (x[i] - x0) / t);
			c4 = c5 - 0.5*(gamma - 1)*(ue[i] - u5);
			pe[i] = p5 * pow(c4 / c5, (2. * gamma / (gamma - 1)));
			rhoe[i] = rho5 * pow(pe[i] / p5, 1.0 / gamma);
		}
		
		// 領域３
		else if (x[i] < xc) {
			rhoe[i] = rho3;
			pe[i] = p3;
			ue[i] = u3;
		}
		
		// 領域２
		else if (x[i] < xs) {
			rhoe[i] = rho2;
			pe[i] = p2;
			ue[i] = u2;
		}
		
		// 領域１
		else {
			rhoe[i] = rho1;
			pe[i] = p1;
			ue[i] = u1;
		}
	}
}


void graph() //　計算結果の可視化グラフを端末に表示
{
	fprintf(fp, "set xrange [-1:1] \n"); ;//横軸表示範囲
	fprintf(fp, "set yrange [-0.1:1.2] \n"); //縦軸表示範囲
	fprintf(fp, "set xtics -1,0.5,1  \n"); //横軸目盛
	fprintf(fp, "set ytics 0,0.5,1  \n"); //縦軸目盛
	fprintf(fp, "plot 0 title 'X-axis'lw 3.5 lc rgb 'black',"); // x軸（黒線）
	fprintf(fp, " '-'title 'Exact sol.' w line lw 2.5  lc rgb 'red',");// 厳密解（赤線）
	fprintf(fp, " '-'title 'Numerical sol.' w point pt 7 ps 1 lc rgb 'blue'\n");// 数値解（青点）
	for (i = 1; i <= i_max - 1; i++) {
		fprintf(fp, "%f %f \n", x[i], rhoe[i]); // 厳密解のデータと送る
	}
	fprintf(fp, "e \n");
	for (i = 1; i <= i_max - 1; i++) {
		//		p[i] = (gamma - 1.0) * (rhoE[i] - 0.5 * rhou[i] * rhou[i] / rho[i]);
		fprintf(fp, "%f %f \n", x[i], rho[i]); // 数値解のデータと送る
	}
	fprintf(fp, "e \n");
	fflush(fp);
}

void graph1() //　計算結果の可視化グラフをファイルに保存
{
	fprintf(fp, "set terminal postscript eps \n");//可視化の出力先指定（epsファイルに出力）
	fprintf(fp, "set output 'output.eps \n");//出力ファイル名指定
	fprintf(fp, "set xrange [-1:1] \n"); ;//横軸表示範囲
	fprintf(fp, "set yrange [-0.25:1.25] \n"); //縦軸表示範囲
	fprintf(fp, "set xtics -1,0.5,1  \n"); //横軸目盛
	fprintf(fp, "set ytics 0,0.5,1  \n"); //縦軸目盛
	fprintf(fp, "plot 0 title 'X-axis'lw 3.5 lc rgb 'black',"); // x軸（黒線）
	fprintf(fp, " '-'title 'Exact sol.' w line lw 2.5  lc rgb 'red',");// 厳密解（赤線）
	fprintf(fp, " '-'title 'Numerical sol.' w point pt 7 ps 1 lc rgb 'blue'\n");// 数値解（青点）
	for (i = 1; i <= i_max - 1; i++) {
		fprintf(fp, "%f %f \n", x[i], rhoe[i]); // 厳密解のデータと送る
	}
	fprintf(fp, "e \n");
	for (i = 1; i <= i_max - 1; i++) {
		//		p[i] = (gamma - 1.0) * (rhoE[i] - 0.5 * rhou[i] * rhou[i] / rho[i]);
		fprintf(fp, "%f %f \n", x[i], rho[i]); // 数値解のデータと送る
	}
	fprintf(fp, "e \n");
	fflush(fp);
}