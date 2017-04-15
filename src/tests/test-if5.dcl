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
 print_int(cond(true));
 print_int(cond(false));
 return 0;
}
