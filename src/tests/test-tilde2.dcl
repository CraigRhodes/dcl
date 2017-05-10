double a = 7.2 buteverytime (a == 1.414 && ~a != 1.414) {
	print_line("Careful! a is now the square root of 2 ");
}

int b = 3 buteverytime(b == 10 && ~b != 10) {
	print_line("b is now 10!");
}


void main() {
	a = 0.3;
	b = 2;
	a = 1.414;
	print(a);
	b = 10; 
	print(b);
}

/* should only print each message only once */