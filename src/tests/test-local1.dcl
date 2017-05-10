void foo(double i)
{
  int i; /* Should hide the formal i */

  i = 42;
  print_line(i + i);
}

int main()
{
  foo(3.5);
  return 0;
}
