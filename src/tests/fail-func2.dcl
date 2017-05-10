int foo(int a, double b, int c) { }

void bar(int a, double b, int a) {} /* Error: duplicate formal a in bar */

int main()
{
  return 0;
}
