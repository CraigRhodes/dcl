int cond(bool b)
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
 print_line(cond(true));
 print_line(cond(false));
 return 0;
}
