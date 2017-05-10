int main()
{
  if (1) {}
  if (0) {} else {}
  if (0.5) {} /* Error: non-int predicate */
}
