//一次元オイラー方程式のLax-Wendroff法
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
FILE *outputfile;           // 実行結果の出力ストリーム

/*
	i=0       i=1                         i=i_max
     |---------|---------|-----   -    ------|---------|
	i=0       i=1                          i=i_max  i=i_max+1
		i番目セルの左境界(x_{i-1/2})の番号はi、
		右境界(x_{i+1/2})のの番号はi+1とする)
															  */

char fname[100];
char csvcont[1000];
int i;                      // セル番号
double x[i_max + 1];        // セル境界の座標
double dx;                  // 格子間隔（等間隔とする）
double dt;                  // 時間刻み
int n;                      // 時間ステップ
double t;                   // 計算時間


double lambda_max;          // 最大特性速度

double rho[i_max];          // 密度“rho”のセル平均値
double rhou[i_max];         // 運動量“rho・u”のセル平均値
double rhoE[i_max];         // 総エネルギー“rho・E”のセル平均値

double rho_star[i_max];     // 密度の中間値
double rhou_star[i_max];    // 運動量の中間値
double rhoE_star[i_max];    // 総エネルギーの中間値

double u[i_max];            // 速度“u”のセル平均値
double p[i_max];            // 圧力“p”のセル平均値

	/* >>>> セル境界における数値流束 >>>> */
double f1[i_max + 1];       // 密度流束
double f2[i_max + 1];       // 運動量流束
double f3[i_max + 1];       // 全エネルギー流束

double rhoe[i_max], ue[i_max], pe[i_max];          //各格子上における密度,速度,圧力の厳密解


		/* >>>> 関数プロトタイプ宣言 >>>> */
double max(double x, double y);       // 実数 x と y の最大値関数
void initc();                         // 初期条件設定関数
void Lax_Wendroff();                  // Lax-Wendroff(高精度数値流束)
void update();                        // 時間発展/時間積分関数
void bc();                            // 境界条件
void cfl_check();                     // CFL条件関数
void exact_solution();                // 厳密解関数
void graph();                         // 結果可視化関数
void graph1();                        // 結果可視化関数（output.epsへ出力）

int main()	//main関数
{
    //fp = _popen("C:/gnuplot/bin/gnuplot", "w"); //描画ツールgnuplotのファイルポインタ指定
	//fprintf(fp, "set terminal windows title 'Solution plots' size 800,600 \n");//可視化の出力先指定（PCの画面に表示）
	fp = popen("/usr/bin/gnuplot", "w"); //描画ツールgnuplotのファイルポインタ指定
    fprintf(fp, "set terminal x11 title 'Solution plots' size 800,600 \n");//可視化の出力先指定（PCの画面に表示）

	initc();                                       // 初期条件設定
	n = 0;                                         // 時間ステップカウンター初期値
	t = 0.0;                                       // 初期時刻

	while (t <= tstop)                             // 時間積分ループ
	{
		n = n + 1;
		t = t + dt;
		cfl_check();	                           // 最大特性速度に基づくCFL条件から
		                                           // 時間積分刻みを決める
		Lax_Wendroff();
		update();
		bc();                                      // 境界条件 （ 0勾配条件 ）
		exact_solution();                          // 厳密解を計算する
		graph();                                   // 計算結果グラフで表示する

        sprintf(fname, "workspace/5-2.%08d.csv", n);
        outputfile = fopen(fname, "w");  // ファイルを書き込み用にオープン(開く)
        if (outputfile == NULL) {          // オープンに失敗した場合
                printf("cannot open\n");         // エラーメッセージを出して
                exit(1);                         // 異常終了
        }
        fprintf(outputfile, "x,rho,u,p,rhou,rhoe\n");
        for (i=0; i <= i_max; i++){
                sprintf(csvcont, "%lf,%lf,%lf,%lf,%lf,%lf\n",
                        x[i], rho[i], u[i], p[i], rhou[i], rhoe[i]);
                fprintf(outputfile, csvcont);
        }
        fclose(outputfile);          // ファイルをクローズ(閉じる)

		printf("n=%d, t=%f, \n", n, t);            // 時間積分回数と時間を表示
	}
	//*****************************************
	graph1();	// 計算結果のグラフを保存する
	//*****************************************
	printf("Press Enter key to finish.\n");
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
	/* >>>> 数値流束の初期値を0にする >>>> */
	for (i = 0; i <= i_max; i++) {
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

void Lax_Wendroff()
{
	double p,p1,p2;
	for (i = 1; i <= i_max - 3; i++) {
		rho_star[i + 1] = 0.5*(rho[i + 1] + rho[i]) - 0.5*dt*(rhou[i + 1] - rhou[i]) / dx;
		p1 = (gamma - 1.0)*(rhoE[i + 1] - 0.5*rhou[i + 1] * rhou[i + 1] / rho[i + 1]);
		p2 = (gamma - 1.0)*(rhoE[i] - 0.5*rhou[i] * rhou[i] / rho[i]);
		rhou_star[i + 1] = 0.5*(rhou[i + 1] + rhou[i])
			- 0.5*dt*((rhou[i + 1] * rhou[i + 1] / rho[i + 1] + p1) - (rhou[i] * rhou[i] / rho[i] + p2)) / dx;
		rhoE_star[i + 1] = 0.5*(rhoE[i + 1] + rhoE[i])
			- 0.5*dt*((rhou[i + 1] * ((rhoE[i + 1] + p1) / rho[i + 1])) - (rhou[i] * ((rhoE[i] + p2) / rho[i]))) / dx;
	}
	for (i = 2; i <= i_max - 2; i++) {
		f1[i] = rhou_star[i];
		p = (gamma - 1.0)*(rhoE_star[i]-0.5*rhou_star[i]*rhou_star[i]/rho_star[i]);
		f2[i] = rhou_star[i]*rhou_star[i]/rho_star[i] + p;
		f3[i] = rhou_star[i]*(rhoE_star[i]+p)/rho_star[i];
	}

}

void update() //　時間発展：次の時刻の保存変数を求める　
{
	/* >>>>　一次オイラー前進時間積分法による保存変数の更新　 >>>> */
	for (i = 2; i <= i_max - 2; i++) {
		rho[i]  = rho[i]  - dt / (x[i + 1] - x[i])*(f1[i + 1] - f1[i]);
		rhou[i] = rhou[i] - dt / (x[i + 1] - x[i])*(f2[i + 1] - f2[i]);
		rhoE[i] = rhoE[i] - dt / (x[i + 1] - x[i])*(f3[i + 1] - f3[i]);
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
	double p21, Vc, Vs, Vrt, Vrh;

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
	pm = 21 + 0.01;
	pmm = pm + 0.01;
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
	} while (error > eps && it <= itmax);
	if (it>itmax)
	{
		printf("反復最大回数を超えた。\n");
	}

	//	領域2の物理量を計算する
	rho2 = rho1*(p21 + (gamma - 1) / (gamma + 1)) /
		((gamma - 1)*p21 / (gamma + 1) + 1);       // 密度
	u2 = u1 + c1*sqrt(2.0 / gamma)*(p21 - 1) /
		sqrt(gamma - 1 + p21 * (gamma + 1));           // 流速
	p2 = p21*p1;                                      // 圧力
													  //	printf("rho2=%f,u2=%f,p2=%f\n",rho2,u2,p2);

													  //	領域3の物理量を計算する
	u3 = u2;
	p3 = p2;
	rho3 = rho5*pow(p3 / p5, 1.0 / gamma);
	c3 = sqrt(gamma * p3 / rho3);
	//	printf("rho3=%f,u3=%f,p3=%f\n", rho3, u3, p3);

	//	各波の速度を計算する
	Vs = u1 + c1*sqrt((gamma + 1) / (2.*gamma)*(p21 - 1) + 1); // 衝撃波
	Vc = u3;                                          // 接触不連続
	Vrt = u3 - c3;                                    // 膨張波末端の速度
	Vrh = u5 - c5;                                      // 膨張波先端の速度
														//	printf("Vs=%f,Vc=%f,Vrt=%f,,Vrh=%f\n", Vs, Vc,Vrt,Vrh);

														// t時刻における波の位置を計算する
	xs = x0 + Vs * t;                                 // 衝撃波
	xc = x0 + Vc * t;                                 // 接触不連続
	xrt = x0 + Vrt * t;                               // 膨張波末端の速度
	xrh = x0 + Vrh * t;                               // 膨張波先端の速度
													  //	printf("xsh=%f,xcd=%f,xft=%f,xhd=%f\n", xs, xc, xrt,xrh);

													  // 計算格子に解を与える
	for (i = 0; i <= i_max - 1; i++) {
		// 領域５
		if (x[i] < xrh) {
			rhoe[i] = rho5;
			pe[i] = p5;
			ue[i] = u5;
		}

		// 領域４
		else if (x[i] <= xrt) {
			ue[i] = 2. / (gamma + 1) * (0.5*(gamma - 1)*u5 + c5 + (x[i] - x0) / t);
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


double max(double x, double y)                     // 2つの実数x,yの最大値を計算する
{
	double z;
	z = x > y ? x : y;
	return (z);

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
