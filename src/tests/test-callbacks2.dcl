int y = 0 buteverytime (y==3) { 
      print_line("y is 3 now!!!");
      y = 0;
}

int main() {
	y = 3; 
	print_line("Testing 1");
	y = 1;
	print_line("Testing 2");
	y = 3;
	y = 2; 
    return 0;
}

/* test setup up of a callback set globally */