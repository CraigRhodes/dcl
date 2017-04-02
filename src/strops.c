#include <stdio.h>
#include <string.h>


// taken from http://stackoverflow.com/questions/14259540/c-programming-strcat-using-pointer
char * addstr(char *x, char *y)
{
	int string_length = strlen(x) + strlen(y);
	char *temporary = (char *)malloc(sizeof(char)* string_length);
	append(temporary, s)
	append(temporary, t)

	return temporary;
}

void append(char *y , char *x) {   
     //move pointer t to end of the string it points. 
    while(*y != '\0'){
        y++;
    }

    while( *x != '\0' ){
        *y = *x;
        y++;
        x++;    
    }       
}  

#ifdef BUILD_TEST
int main()
{
  printf("\"swe\" + \"et!\" == %s\n", addstr("swe", "et!"));
  return 0;
}
#endif
