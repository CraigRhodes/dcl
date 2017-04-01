void foo(int a, bool b)
{
}

int main()
{
  foo(42, true);
  foo(42, true, 0); /* Wrong number of arguments */
}
