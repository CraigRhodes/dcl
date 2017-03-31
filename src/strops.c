#include <stdio.h>

char * addstr(char *x, char *y)
{
	return "";
}

#ifdef BUILD_TEST
int main()
{
  printf("\"swe\" + \"et!\" == %s\n", addstr("swe", "et!"));
  return 0;
}
#endif
