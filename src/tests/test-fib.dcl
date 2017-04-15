int fib(int x)
{
  if (x < 2) return 1;
  return fib(x-1) + fib(x-2);
}

int main()
{
  print_int(fib(0));
  print_int(fib(1));
  print_int(fib(2));
  print_int(fib(3));
  print_int(fib(4));
  print_int(fib(5));
  return 0;
}
