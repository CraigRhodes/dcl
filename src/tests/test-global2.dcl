bool i;

int main()
{
  int i; /* Should hide the global i */

  i = 42;
  print_int(i + i);
  return 0;
}
