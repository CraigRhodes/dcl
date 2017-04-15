void foo(bool i)
{
  int i; /* Should hide the formal i */

  i = 42;
  print_int(i + i);
}

int main()
{
  foo(true);
  return 0;
}
