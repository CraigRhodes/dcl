#include <math.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>


double exp_int(int x, int y)
{
  return pow(x, y);
}

double exp_dbl(double x, double y)
{
  return pow(x, y);
}

char * add_str(char *x, char *y)
{
	int total_length = strlen(x) + strlen(y) + 1;
	char *result = (char *) malloc(total_length);
	strcpy(result, x);
	strcat(result, y);
	return result;
}

#ifdef BUILD_TEST
int main()
{
  printf("5 ^ -2 == %f\n", exp_int(5, -2));
  printf("5 ^ 0.5 == %f\n", exp_dbl(5.0, 0.5));
  printf("\"swe\" + \"et!\" == \"%s\"\n", add_str("swe", "et!"));
  return 0;
}
#endif



