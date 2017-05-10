void main() {
	int[] as;
	int[] bs;
	int i;
	as = [1, 2, 3, 4];
	bs = as;
	for(i = 0; i < #as; i = i + 1) {
		print(as {| i |});
		print(" ");
		print_line(bs {| i |});
	}
	as = [6, 7, 8, 9];
	for(i = 0; i < #as; i = i + 1) {
		print(as {| i |});
		print(" ");
		print_line(bs {| i |});
	}
}