void foo(int a, double b)
{
}

int main()
{
  foo(42, 23.3);
  foo(42, 42); /* Fail: int, not double */
}
