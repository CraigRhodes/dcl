bool i;

int main()
{
  int i; /* Should hide the global i */

  i = 42;
  print_line(i + i);
  return 0;
}
