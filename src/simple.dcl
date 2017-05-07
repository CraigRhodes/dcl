int return_two_times_two() {
	return 4;
}

void main() {
	double[] fs; 
	int[] ds;
	int i;
	string[] ss;
	int[] ns;
	fs = [1., 2., 33.0 ^ 0.5, 5.6];
	ds = [10, 2, 33, 41];
	ns = [return_two_times_two() of return_two_times_two()];
	ss = ["Hi_world!", "\"hi there\"", "Bro bro bro!", "\'hi there\'"];
	fs[ 1 ] = 17.0 * 0.33;
	ds[ 1 ] = 5;
	ss[ 1 ] = "DLDLDKDKD";
	print(ds{| 0 |});
	for(i = 0; i < #ss; i = i + 1) {
		print_line(fs{| i |});
		print_line(ds{| i |});
		ns[ i ] = ns {| i |} + 7;
		print_line(ns{| i |});
		print_line(ss{| i |});
	    print_line(ss{| 0 |}{| i |});
	}
	print_line(#(ss{| 0 |}));
	print_line("New line, best line");
	print_line("Hi" + " world!");
}