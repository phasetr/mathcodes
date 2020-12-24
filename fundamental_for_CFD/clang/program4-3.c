/* 一次元線形移流方程式の二次精度Lax-Wendroff法 */
#define _USE_MATH_DEFINES
#include <stdio.h>
#include <math.h>
#include <stdlib.h>

#define i_max 100           //格子セル数
#define XL -1.0            //計算領域左端の座標
#define XR 1.0            //計算領域右端の座標
#define a 1.0             //線形移流方程式の移流速度
#define tstop 2.0            //計算停止時刻
FILE *fp;				 // 描画ソフトgnuplotの実行ファイルのポインタ

int i;    //セル番号； (i番目セルの左境界の番号はi、右辺境界の番号はi+1とする)
double x[i_max + 1];    //セル境界の座標
double u[i_max];       //セル平均値 （数値解）
double ue[i_max];      //セル平均値 （厳密解）
double ul[i_max + 1];    //セル境界左側の変数値
double ur[i_max + 1];    //セル境界右側の変数値
double f[i_max + 1];    //セル境界の流束
double dx;            //格子間隔（等間隔とする）
double dt;            //時間刻み
int n;            //時間ステップ
double t;            //計算時間

/*
　    計算変数と数値流速の配置

	 セル境界　　　　　 セル境界
  -------|-----------------|-------
	   f[i]     u[i]    f[i+1]　　　==> 計算コードの変数定義

	  f_i-1/2   u_i     f_i+1/2     ==> テキストの標記


　  　境界条件
	u[0]     u[1]                    u[i_max-2]  u[i_max-1]
 ---------|-------|  ---- ---  ----|-----------|---------
		 [1]     [2]           [i_max-2]    [i_max-1]
			  計算境界		    計算境界

*/

void graph(); //gnuplotを用いて描画する関数
void graph1(); //gnuplotを用いて描画する関数（output.epsへ出力）
void initc(int); //初期条件を設定する関数
void reconstruction_lw();//再構築関数（１次補間，Lax-Wendroff法）
void riemann_roe();//リーマン・ソルバー関数（Roeスキーム）
void update();//時間前進を計算する関数
void bc();//境界条件を設定する関数（周期境界条件）
void exact(int);//厳密解を計算する関数

int main() //　main　関数
{
	int sw1;
	fp = _popen("C:/gnuplot/bin/gnuplot", "w"); //描画ツールgnuplotのファイルポインタ指定
	fprintf(fp, "set terminal windows title 'Solution plots' size 800,600 \n");//可視化の出力先指定（PCの画面に表示）

	printf("線形移流方程式の初期値を選ぶ\n");
	printf("滑らかな分布（正弦波）：１　\n");
	printf("不連続な分布（矩形波）：２　\n");
	scanf_s("%d", &sw1);

	n = 0;
	t = 0.0;
	initc(sw1);	// 計算格子，時間刻み，初期条件を設定する
	exact(sw1);
	graph();	// 計算結果をグラフで表示する
	getchar();

	while (t <= tstop) //　メインループ
	{
		n = n + 1;
		t = t + dt;
		reconstruction_lw();	// 空間再構築
		riemann_roe();	// リーマンソルバー
		update();	// 時間積分
		bc();	// 境界条件
		exact(sw1);// 厳密解
		graph();	// 計算結果をグラフで表示する
		printf("n=%d, t=%f, \n", n, t);
	}
    //*****************************************
    graph1();	// 計算結果のグラフを保存する
    //*****************************************
	printf("Press Enter key to finish.\n");
    getchar();
}

void initc(int sw1) { // 計算格子，時間刻み，初期条件を設定する
	dx = (XR - XL) / ((double)i_max - 4.0);  //　格子間隔
	dt = 0.2 * dx; //時間刻み
	x[0] = XL - 2.0 * dx;
	for (i = 1; i <= i_max; i++) {
		x[i] = x[i - 1] + dx;  //　格子点の座標
	}

	for (i = 0; i <= i_max; i++) {
		ul[i] = 0.0;  //セル境界左側の変数値を初期化
		ur[i] = 0.0;  //セル境界右側の変数値を初期化
		f[i] = 0.0;  //セル境界の数値流束を初期化
	}

	for (i = 0; i <= i_max - 1; i++) {
		u[i] = 0.0;  //　変数値を０に初期化
	}

	switch (sw1)
	{
	case 1:		//　初期の変数値（滑らかな分布）
		for (i = 0; i <= i_max - 1; i++) {
			u[i] = 0.5 * (1.1 + sin(2. * M_PI * (x[i] - x[2])));
		}
		break;
	case 2:		//　初期の変数値（不連続な分布）
		for (i = 0; i <= i_max - 1; i++) {
			u[i] = 0.1;
		}
		for (i = i_max / 2 - 10; i <= i_max / 2 + 10; i++) {
			u[i] = 1.0;
		}
		break;
	default:
		printf("初期値を正しく選択されていません。");
		break;
	}
}

void reconstruction_lw() {
    double alpha_12, deltaL, deltaR;
    for (i = 1; i <= i_max - 3; i++) {
         alpha_12 = a;
         deltaL = 0.5*(u[i + 1] - u[i]);
        deltaR = 0.5*(u[i + 1] - u[i]);
        ul[i + 1] = u[i] + (1.0 - alpha_12*dt / dx)*deltaL;  //セル境界(i+1/2)左側の値
        ur[i + 1] = u[i + 1] - (1.0 + alpha_12*dt / dx)*deltaR;  //セル境界(i+1/2)右側の値
    }

}

void riemann_roe() {
	//	流束を計算する
	for (i = 2; i <= i_max - 2; i++) {
		f[i] = 1.0 / 2.0*(a*ul[i] + a * ur[i]) - 1.0 / 2.0*fabs(a)*(ur[i] - ul[i]);
	}
}

void update() {
	for (i = 2; i <= i_max - 3; i++) {
		u[i] = u[i] - dt / dx * (f[i + 1] - f[i]);//計算変数を更新する
	}
}

void exact(int sw1) {
	double xc, xl, xr;
	switch (sw1)
	{
	case 1:		//　初期の変数値（滑らかな分布）
		for (i = 0; i <= i_max - 1; i++) {
			ue[i] = 0.5 * (1.1 + sin(2. * M_PI * ((x[i] - x[2]) - a * t)));
		}
		break;

	case 2:		//　初期の変数値（不連続な分布）
		xc = a * t;
		if (xc < -1.0)
		{
			xc = 2.0 + xc;
		}
		xl = xc - dx * 10.;
		if (xl > 1.0 - dx * 0.0)
		{
			xl = -2.0 + xl + dx * 0.0;
		}
		xr = xc + dx * 10.;
		if (xr > 1.0 - dx * 0.0)
		{
			xr = -2.0 + xr + dx * 0.0;
		}
		if (xl <= xr)
		{
			for (i = 0; i <= i_max - 1; i++) {
				ue[i] = 0.1;
				if (dx * double(i - i_max / 2) >= xl && dx * double(i - i_max / 2) <= xr)
				{
					ue[i] = 1.0;
				}
			}
		}
		if (xl >= xr)
		{
			for (i = 0; i <= i_max - 1; i++) {
				ue[i] = 1.0;
				if (dx * double(i - i_max / 2) >= xr && dx * double(i - i_max / 2) <= xl)
				{
					ue[i] = 0.1;
				}
			}
		}
		break;
	}
}

void bc() {    //周期境界条件
	u[0] = u[i_max - 4];  //　計算領域左端の境界条件
	u[1] = u[i_max - 3];  //　計算領域左端の境界条件
	u[i_max - 2] = u[2];    //　計算領域右端の境界条件
	u[i_max - 1] = u[3];      //　計算領域右端の境界条件
}

void graph() //　計算結果の可視化グラフを端末に表示
{

	fprintf(fp, "set xrange [-1:1] \n"); ;//横軸表示範囲
	fprintf(fp, "set yrange [-0.25:1.25] \n"); //縦軸表示範囲

	fprintf(fp, "set xtics -1,0.5,1  \n"); //横軸目盛
	fprintf(fp, "set ytics 0,0.5,1  \n"); //縦軸目盛

	fprintf(fp, "plot 0 title 'X-axis'lw 3.5 lc rgb 'black',"); // x軸（黒線）
	fprintf(fp, " '-'title 'Exact sol.' w line lw 2.5  lc rgb 'red',");// 厳密解（赤線）
	fprintf(fp, " '-'title 'Numerical sol.' w point pt 7 ps 1 lc rgb 'blue'\n");// 数値解（青点）
	for (i = 1; i <= i_max - 1; i++) {
		fprintf(fp, "%f %f \n", x[i], ue[i]);  // 厳密解のデータと送る
	}
	fprintf(fp, "e \n");
	for (i = 1; i <= i_max - 1; i++) {
		fprintf(fp, "%f %f \n", x[i], u[i]);  // 数値解のデータと送る
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
		fprintf(fp, "%f %f \n", x[i], ue[i]);  // 厳密解のデータと送る
	}
	fprintf(fp, "e \n");
	for (i = 1; i <= i_max - 1; i++) {
		fprintf(fp, "%f %f \n", x[i], u[i]);  // 数値解のデータと送る
	}
	fprintf(fp, "e \n");
	fflush(fp);
}