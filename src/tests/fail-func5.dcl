int foo() {}

int bar() {
  int a;
  void b; /* Error: illegal void local b */
  double c;

  return 0;
}

int main()
{
  return 0;
}
