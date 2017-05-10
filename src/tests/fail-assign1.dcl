int main()
{
  int i;
  double b;

  i = 42;
  b = 25.7;
  i = 25.7; /* Fail: assigning a double to an integer */
}
