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
 print_bool(cond(true));
 print_bool(cond(false));
 return 0;
}
