int foo(int a, double b)
{
  int c;
  double d;

  c = a;

  return c + 10;
}

int main() {
 print_line(foo(37, 3.5));
 return 0;
}
