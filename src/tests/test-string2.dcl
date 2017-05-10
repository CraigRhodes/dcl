void main() {
	string g;
	string[] h;
	string i;
	g = "hello";
	h = ['world', 'ghana'];
        i = (h{|0|}) + g;
	print_line(i);
}

/*should work because string + string concatenation is supported */
