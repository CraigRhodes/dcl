void foo() {}

int bar(int a, int c) { return a + c; }

int main()
{
  print_int(bar(17, 25));
  return 0;
}
