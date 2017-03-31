#include <math.h>
#include <stdio.h>

double exp_int(int x, int y)
{
  return pow(x, y);
}

double exp_dbl(double x, double y)
{
  return pow(x, y);
}

#ifdef BUILD_TEST
int main()
{
  printf("5 ^ -2 == %f\n", exp_int(5, -2));
  printf("5 ^ 0.5 == %f\n", exp_dbl(5.0, 0.5));
  return 0;
}
#endif
