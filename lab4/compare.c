/* Joseph Maples
   CS-231 02L
   Assignment 3 - Dictionary comparator
 */

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

FILE* open_file(char *prog_name, char *file_name) {
/*  open_file opens a pointer to the requested file name
    and makes sure the file exists. If the file does not
    exist, an error is printed and execution stops. If it
    does exits, the function returns the pointer.

data table

NAME               DESCRIPTION
prog_name          parameter - the value of argv[0].
file_name          parameter - the value of argv[1], a file name
file_pointer       variable - pointer to the file file_name.

*/
    FILE *file_pointer = fopen(file_name, "r"); // Open file in read mode

    // Handle non-existent file
    if (file_pointer == NULL) {
       fprintf(stderr, "%s: %s: No such file for directory\n", prog_name, file_name);
       exit(1);
    }

    return file_pointer;
}

int is_whitespace(const char character) {
/*  is_whitespace checks whether a given char is a space, tab,
    newline, or 0-byte char. Returns 1 if true, 0 if false.

data table

NAME               DESCRIPTION
character          parameter - the char being checked.

*/
    if (character == ' ' || character == '\t' ||
        character == '\0' || character == '\n') {
        return 1;
    }
    return 0;
}

char *to_lower(char *word) {
/*  to_lower converts all chars in a string to lowercase,
    and returns the lower case string.

data table

NAME               DESCRIPTION
word               parameter - word to make lowercase.
length             variable - length of word.
lower_word         variable - the lowercase word.

*/
    int length = strlen(word);
    char *lower_word = malloc(length);
    for (int letter = 0; letter < length; letter++)
        lower_word[letter] = (char)tolower(word[letter]);

    return lower_word;
}

void compare_words(FILE *dict) {
/*  compare_words takes a line from standard in, which 
    should be a single word, and compares it to every word
    in the provided dictionary file until a match is found
    or we run out of words to compare. If a word does not
    match, it is printed to stdout.

data table

NAME               DESCRIPTION
dict               parameter - pointer to the dictionary file.
dict_word          variable - a word from the dictionary file.
word               variable - a word from stdin.
is_word            variable - boolean-style int for recording
                              whether a word was spelled
                              correctly or not. 

*/
    char dict_word[20];
    char word[20];
    while(fgets(word, 20, stdin) != NULL) {
        int is_word = 0;
        while(fgets(dict_word, 20, dict)) {
            if (!strcmp(to_lower(word), to_lower(dict_word))) {
                is_word = 1;
                break;
            }
        } 
        rewind(dict);
        if (is_word)
            printf("Correct: %s", word);
        else
            printf("Incorrect: %s", word);
    }
}

int main(int argc, char *argv[]) {
/*  main is the program's controller function. It first
    verifies that the correct number of arguments have
    been supplied. Next, it opens a file pointer and
    compares the words within it. Finally, it closes the
    file pointer and returns.

data table

NAME               DESCRIPTION
argc               parameter - the number of arguments supplied.
argv               parameter - the arguments supplied.
file               variable - pointer to the file name at argv[1].

*/
    if (argc < 2) {
        fprintf(stderr, "%s: No file supplied\n", argv[0]);
        exit(1);
    } else if (argc > 2) {
        fprintf(stderr, "%s: More than one file supplied\n", argv[0]);
        exit(1);
    }
    FILE *file = open_file(argv[0], argv[1]);
    compare_words(file);
    fclose(file);
    return 0;
}