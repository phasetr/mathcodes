float a = 0.7;
float b = 0.7;
float[] u0;
float[] v0;
float[] u1;
float[] v1;
float[] s; // sigma
int step = 100;
float w = 0;
int time = 0;

void setup() {
  size(600, 300);
  init();
  //noSmooth();
  frameRate(20);
  background(0);
  fill(0, 0, 0, 0);
  stroke(255, 255, 255);
}

void draw() {
  background(0);
  for (int i=0; i<step; i++) {
    float x1,x2,y1,y2;
    x1 = w*i;
    y1 = height/2+u1[i];
    ellipse(x1, y1, 2, 2);
    if (i < step-1) {
      y2 = height/2+u1[i+1];
      x2 = w*(i+1);
      line(x1, y1, x2, y2);
    }
  }
  for (int i=0; i<1; i++) // speed up
    updateTime();
}

void init()
{
  w = width/(step-1);
  u0 = new float[step];
  v0 = new float[step];
  u1 = new float[step];
  v1 = new float[step];
  s = new float[step];
  int llen = step/2; // length of layer
  float inc_step = 0.5/llen;
  for (int i=step-llen; i<step; i++) {
    s[i] = inc_step*(i-(step-llen));
    println(s[i]);
  }
}

void updateTime()
{
  int force_zero_len = 5;
  u1[0] = 100*sin(2*PI/20*time);

  for (int i=1; i<step-1; i++) {
    u1[i]  = u0[i] + b*(v0[i] - v0[i-1]) - s[i]*u0[i];
  }
  for (int i=0; i<step-1; i++) {
    v1[i]  = v0[i] + a*(u1[i+1] - u1[i]) - s[i]*v0[i];
  }
  for (int i=0; i<step-force_zero_len; i++) {
    u0[i] = u1[i];
    v0[i] = v1[i];
  }

  time++;
}
