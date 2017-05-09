int fib(int x)
{
  if (x < 2) return 1;
  return fib(x-1) + fib(x-2);
}

int main()
{
  print_line(fib(0));
  print_line(fib(1));
  print_line(fib(2));
  print_line(fib(3));
  print_line(fib(4));
  print_line(fib(5));
  return 0;
}
