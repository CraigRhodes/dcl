/* Bee Movie but every time they say "bee"... */

/*string[] split(string whole, string sep) {
	...
}*/

string join(string[] parts, string sep) {
	string total = "";
    for(int i = 0; i < #parts; i = i + 1) {
        total = total + parts{| i |} + sep;
    }
    return total;
}

string current_word = "" buteverytime (current_word == "bee") {
    current_word = "b++";
}

void main() {
    print_line(join(["Craig", "doo", "be", "do"], " "));
    /*string bee_movie_script = read("bee_movie_script.txt");
    
    string[] bee_movie_words = split(bee_movie_script, " ");
    string[] modified_bee_movie_words = [length(bee_movie_words) of string];
    
    for(int i = 0; i < length(bee_movie_words); i = i + 1) {
        current_word = bee_movie_words[i];
        modified_bee_movie_words[i] = current_word;
    }
    
    write("b++_movie_script.txt", join(modified_bee_movie_words, " "));*/
}

/* Would be cool if we fit this on one slide and then highlight it for different parts:
     * Variable declaractions
     * Buteverytime
     * Being able to define your own functions that are non-trivial
     * File I/O
     * Arrays
     * Strings
 */
/* First slide is scene from Bee Movie */