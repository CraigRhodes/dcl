void foo(int a, double b)
{
}

int main()
{
  foo(42, 23.4);
  foo(42, 23.5, 636.1); /* Wrong number of arguments */
}
