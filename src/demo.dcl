/* Bee Movie but every time they say "bee"... */

string[] split(string whole, string sep) {
	int number_of_parts = 0;
    int looking_for_parts = 0;
    int index;
    for(index = 0; index < #whole; index = index + 1) {
        if(whole{| index |} == sep) {
            if(looking_for_parts) {
                looking_for_parts = 0;
            }
        } else {
            if(!looking_for_parts) {
                number_of_parts = number_of_parts + 1;
                looking_for_parts = 1;
            }
        }
    }
    string[] parts = [number_of_parts of ""];
    int current_index = 0;
    looking_for_parts = 0;
    string current = "";
    for(index = 0; index < #whole; index = index + 1) {
        if(whole{| index |} == sep) {
            if(looking_for_parts) {
                looking_for_parts = 0;
                parts[ current_index ] = current;
                current_index = current_index + 1;
                current = "";
            }
        } else {
            if(!looking_for_parts) {
                looking_for_parts = 1;
            }
            current = current + whole{| index |};
        }
    }
    parts[ current_index ] = current;
    return parts;
}

string join(string[] parts, string sep) {
	string total = "";
    for(int part = 0; part < #parts; part = part + 1) {
        total = total + parts{| part |} + sep;
    }
    return total;
}

int starts_with(string haystack) {
    return (haystack{| 0 |} == 'b' || haystack{| 0 |} == 'B') &&
           haystack{| 1 |} == 'e' && haystack{| 2 |} == 'e';
}

string current_word = "" buteverytime (starts_with(current_word)) {
    current_word = "     b + +     ";
}

void main() {
    string bee_movie_script = read("bee_movie_script.txt");
    
    string[] bee_movie_words = split(bee_movie_script, " ");
    string[] modified_bee_movie_words = [#bee_movie_words of ""];
    
    for(int i = 0; i < #bee_movie_words; i = i + 1) {
        current_word = bee_movie_words{|i|};
        modified_bee_movie_words[i] = current_word;
    }
    
    write("b++_movie_script.txt", join(modified_bee_movie_words, " "));
}