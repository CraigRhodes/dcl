int main()
{
  int i;

  while (1) {
    i = i + 1;
  }

  while (32.5) { /* Should be boolean */
    i = i + 1;
  }

}
