/* Joseph Maples
   CS-231 02L
   Assignment 4 - File Lexer
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

void print_words(FILE *file) {
/*  print_words simply prints all alphabetic chars, or a \n
    when the char is not alphabetic. Thanks for making me
    feel dumb Dr. Vineyard.

data table

NAME               DESCRIPTION
file               parameter - the file being checked.
cur_char           variable - a char from the file.
prev_char          variable - the previous char.

*/
    char cur_char;
    char prev_char;
    while((cur_char = fgetc(file)) != EOF) {
        if (isalpha(cur_char))
            printf("%c", cur_char);
        else if (isalpha(prev_char))
            printf("\n");
        prev_char = cur_char;
    }  
}

int main(int argc, char *argv[]) {
/*  main is the program's controller function. It first
    verifies that the correct number of arguments have
    been supplied. Next, it opens a file pointer and
    prints the words within it. Finally, it closes the
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
    print_words(file);
    fclose(file);
    return 0;
}