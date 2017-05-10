string new_thing = "Hi" buteverytime (new_thing == "bye") {
	print_line("IT changed!");
}

int i = 0 buteverytime (i == 42) {
	i = -13;	
}

void main() {
	/*string stuff = read("hello.txt");
	print_line("\"" + stuff + "\"");
	string new_stuff = "Craig is talking about how DCL works";
	print_line(new_stuff);
	int num_written = write("out.txt", new_stuff);
	print(num_written); print_line(" characters written");
	new_thing = "bye";*/

	int[] is = [4, 42, 2, 3, 42, 5, 42];
	for(int index = 0; index < #is; index = index + 1) {
		i = is {| index |};
		print_line(i);
	}
}