int main()
{
  int i;
  i = 0;
  for ( ; i < 5; ) {
    print_line(i);
    i = i + 1;
  }
  print_line(42);
  return 0;
}
