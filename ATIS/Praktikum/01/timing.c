#include <stdio.h>

#ifdef WIN32
  #include <windows.h>
  double get_time() {
    LARGE_INTEGER t, f;
    QueryPerformanceCounter(&t);
    QueryPerformanceFrequency(&f);
    return (double)t.QuadPart / (double)f.QuadPart;
  }
#else
  #include <sys/time.h>
  #include <sys/resource.h>

  double get_time() {
    struct timeval t;
    struct timezone tzp;
    gettimeofday(&t, &tzp);
    return t.tv_sec + t.tv_usec * 1e-6;
  }
#endif

extern int v1();
extern int v2();
extern int v3();
extern int v4();

typedef int(*func)();
int(*funcs[4])() = {&v1, &v2, &v3, &v4};

int main() {
  const int num_iter = 10000000;
  for (int v = 0; v < 4; v++) {
    double start = get_time();
    volatile long sum_real = 0;
    for (int i = 0; i < num_iter; i++) {
      sum_real += funcs[v]();
  }
  double time = get_time() - start;
  printf("v%d = %f msec\n", v + 1, time*1000);
  }
}
