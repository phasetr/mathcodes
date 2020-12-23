#include <iostream>
#include <cmath>
#include <fstream>
#include <iomanip>
#include <string>
#include <sstream>

using namespace std;


inline void solve(double x[], int &Node, double &dt, double &dx, double &V)
{
	int i;

	for(i=1;i<Node-1;i++)
	{
		x[i]=x[i]-dt/dx*(-max(V,0.0)*x[i-1]+(max(V,0.0)+max(-V,0.0))*x[i]-max(-V,0.0)*x[i+1]);
	}

}

inline void boundary(int &bc, double &D0, double x[], double &V, int &Node, double &dt, double &dx)
{
	if(bc==1)
	{
		x[0]=D0;
		x[Node-1]=x[Node-1]-dt/dx*V*(x[Node-1]-x[Node-2]);
	}
	if(bc==2)
	{
		x[0]=x[0]-dt/dx*V*(x[1]-x[0]);
		x[Node-1]=D0;
	}
}

inline void text(int &i,double x[], double &dx, int &Node)
{
	int j;

	stringstream ss;
	string name;
	ofstream fo;

	ss<<i;
	name=ss.str();
	name="Answer_" + name + ".tmp.txt";
	fo.open(name.c_str ());

	for(j=0;j<Node;j++)
	{
		fo<<dx*float(j)<<" "<<x[j]<<endl;
	}
}

int main()
{
	int i;
	int Partition=400;
	int Node=Partition+1;
	double LL=1.0;
	double dx=LL/(Node-1);
	double dt=0.001;
	double NT=7000;
	double eps=pow(2.0,-50);
	double V=0.1;
	int bc=1;//bc=1 left Dirichlet (V>=0), bc=2 right Dirichlet (V<0)
	double D0=0.0;//for Dirichlet
	double *x=new double[Node];

	ofstream fk;
	fk.open("Answer_0000.tmp.txt");

	//initial condition//
	for(i=0;i<Node;i++)
	{
		x[i]=0.0;
		if((i>=40)&&(i<=80)) x[i]=1.0;
		fk<<dx*float(i)<<" "<<x[i]<<endl;
	}

	for(i=1;i<=NT;i++)
	{
		solve(x,Node,dt,dx,V);
		boundary(bc,D0,x,V,Node,dt,dx);

		if(i%1000==0)
		{
			cout<<i<<endl;
			text(i,x,dx,Node);
		}
	}

	delete[] x;

	return 0;
}
