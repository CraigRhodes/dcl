int a;

void foo(int c)
{
  a = c + 42;
}

int main()
{
  foo(73);
  print_line(a);
  return 0;
}
