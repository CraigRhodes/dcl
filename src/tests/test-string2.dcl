void main() {
	string g;
	string[] h;
	string i;
	g = "hello";
	h = ['world', 'ghana'];
	i = g + h{|0|};
	print_line(i);
}

/*should work because string + string concatenation is supported */