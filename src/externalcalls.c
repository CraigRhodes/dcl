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

char * read(const char *filename) {
  FILE *fp;
  int len;
  char *buffer;
  fp = fopen(filename, "rb");
  if(fp == NULL) {
    perror("failed to open file");
    exit(1);
  }
  fseek(fp, 0, SEEK_END);
  len = ftell(fp);
  buffer = malloc(sizeof(char) * (len + 1));
  if(buffer == NULL) {
    perror("malloc failed");
    exit(1);
  }
  fseek(fp, 0, SEEK_SET);
  fread(buffer, len, sizeof(char), fp);
  fclose(fp);

  return buffer;
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



