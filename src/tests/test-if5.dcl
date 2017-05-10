int cond(int b)
{
  int x;
  if (b)
    x = 42;
  else
    x = 17;
  return x;
}

int main()
{
 print_line(cond(1));
 print_line(cond(0));
 return 0;
}
