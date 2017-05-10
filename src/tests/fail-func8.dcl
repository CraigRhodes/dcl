void foo(int a, double b)
{
}

void bar()
{
}

int main()
{
  foo(42, 32.5);
  foo(42, bar()); /* int and void, not int and double */
}
