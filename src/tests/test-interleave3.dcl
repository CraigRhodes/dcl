string hello_world() {
	print_line("hello world!");
	return "hello world!";
}

int main() {
	int a; 
	int b;
	a = 5;
	b = 10;
	hello_world();
	string s;
	s = "hello world again!";
	print_line(s);

}

/* verifying that interleaving of initializing variables and calling functions */
