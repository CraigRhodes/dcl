#include <stdio.h>
#include <string.h>
#include <stdlib.h>

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
  printf("\"swe\" + \"et!\" == \"%s\"\n", add_str("swe", "et!"));
  return 0;
}
#endif
