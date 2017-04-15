int a;

void foo(int c)
{
  a = c + 42;
}

int main()
{
  foo(73);
  print_int(a);
  return 0;
}
