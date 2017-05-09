int gcd(int a, int b) {
  while (a != b)
    if (a > b) a = a - b;
    else b = b - a;
  return a;
}

int main()
{
  print_line(gcd(14,21));
  print_line(gcd(8,36));
  print_line(gcd(99,121));
  return 0;
}
