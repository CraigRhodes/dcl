int a;
int b;

void printa()
{
  print_line(a);
}

void printb()
{
  print_line(b);
}

void incab()
{
  a = a + 1;
  b = b + 1;
}

int main()
{
  a = 42;
  b = 21;
  printa();
  printb();
  incab();
  printa();
  printb();
  return 0;
}
