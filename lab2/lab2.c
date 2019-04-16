/* Joseph Maples
   CS-231 02L
   Assignment 2 - Handling string arrays
 */

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define NAME      "Assignment 2" // Program name
#define DESC      "Handling string arrays" // Program description
#define MAX_CHARS 101 // 100 characters plus "\0" or "\n".
#define MAX_LINES 100 // Maximum number of input lines allowed.

void print_prog_info(const char *title, const char *desc) {
/*  print_prog_info prints the title, author, class period,
    and description of the program.

data table

NAME               DESCRIPTION
title              parameter - the title of the output.
desc               parameter - identifies programming task.

*/
    printf("%s\n\n", title);
    printf("Joseph Maples\nCS-231 02L\n%s\n\n", desc);
}

int get_input_lines(char page[MAX_LINES][MAX_CHARS]) {
/*  get_input_lines assigns lines of user input to indicies of
    the given char pointer array "page" and returns the
    total number of lines the user input.

data table

NAME               DESCRIPTION
page               parameter - array to store user_input in.
buffer             variable - temporary user_input storage.
lines              variable - total number of lines user_input.

*/
    char buffer[MAX_CHARS];

    int lines = 0;
    while (lines < MAX_LINES && fgets(buffer, MAX_CHARS, stdin) != NULL) {
        strcpy(page[lines], buffer); // Copy string from the buffer
        lines++;
    }
    return lines;
}

int *count_letters(const char line[MAX_CHARS]) {
/*  count_letters counts the number of each letter in a given
    string. Each char is tested using its numeric ascii value
    and the count is stored in an int array of size 26.

data table

NAME               DESCRIPTION
line               parameter - the line of text to count.
num_letters        variable - array that stores the letter count.
character          variable - index of current character.
ascii_value        variable - the numeric ascii value of the char.
letter             variable - ascii value without non-alpha chars.

*/
    int *num_letters = (int*) malloc(26 * sizeof(int));

    for(int character = 0; character < MAX_CHARS; character++)
    {
        if(isalpha(line[character])) {
            int ascii_value = (int) line[character]; // Use numeric ascii value

            // If letter is lowercase, make it upper
            if(ascii_value > 90)
                ascii_value = ascii_value - 32;

            int letter = (int) (ascii_value - 65); // Remove preceeding ascii values to be 0-25

            // Increment the found letter
            num_letters[letter]++;
        } else if (line[character] == '\0' || line[character] == '\n')
            break;
    }
    return num_letters;
}

void print_stats(const int words, const int *num_letters) {
/*  print_stats outputs a word count and letter count in a well
    formed string. It checks the value of the letter and word
    counts to maintain correct plurality (is/are and s suffix).

data table

NAME               DESCRIPTION
words              parameter - the number of words.
num_letters        parameter - array of the number of letters.
first_letter       variable - boolean if this is the first letter with
                              a non-zero count.
output             variable - the formated output string.
buffer             variable - temporary storage for formating.
letter             variable - index of letter count being checked.
character          variable - letter index converted to char.
is_are             variable - set to "is" or "are" depending on plurality.

*/
    int first_letter = 0;
    char output[MAX_CHARS * 2];
    char buffer[14]; // 14 is the max length of one buffer string.

    // Only count letters if they exist
    if (words > 0) {
        for (int letter = 0; letter < 26; letter++) {
            // Only output used letters
            if (num_letters[letter] > 0) {
                // Format the number of the current letter into a string
                char character = (char) letter + 65;

                // Make sure plurality is correct
                if (first_letter == 0) {
                first_letter = 1;
                    char *is_are = num_letters[letter] == 1 ? "is " : "are ";
                    strcpy(output, is_are);
                }
                if (num_letters[letter] == 1)
                        sprintf(buffer,"%d %c, ", num_letters[letter], character);
                else
                        sprintf(buffer,"%d %c\'s, ", num_letters[letter], character);

                strcat(output, buffer); // Add buffer to output
                buffer[0] = '\0'; // Empty buffer when done
            }
        }

        // Format number of words into a string
        sprintf(buffer,"in %d words.", words);
        strcat(output, buffer);
    } else {
        // If the line has 0 words, only print that.
        strcpy(output, "are 0 words.");
    }

    // Print the line analysis
    printf("There %s\n", output);
}

int *add_int_arrays(const int *one, const int *two, const int size) {
/*  add_int_arrays takes two int arrays of the same size and adds
    the values at each index.

data table

NAME               DESCRIPTION
one                parameter - one of the two int arrays.
two                parameter - the other int array.
size               parameter - the size of the arrays.
final              variable - the sum of the two arrays.
index              variable - the index that is being added.

*/
    int *final = malloc(size * sizeof(int));
    for (int index = 0; index < size; index++)
        final[index] = one[index] + two[index];

    return final;
}

int is_whitespace(const char character) {
/*  is_whitespace checks whether a given char is a space, tab,
    newline, or 0-byte char. Returns 1 if true, 0 if false.

data table

NAME               DESCRIPTION
character          parameter - the char being checked

*/
    if (character == ' ' || character == '\t' ||
        character == '\0' || character == '\n') {
        return 1;
    }
    return 0;
}

int get_words_print_reverse(const char *line) {
/*  get_words_print_reverse counts the total number of words
    in a line of text, and reverses each of those words while
    maintaining its position in the line. The reversed line
    is printed and the number of words is returned.

    The words are counted and reversed by looping over each
    char and checking for whitespace imediately after a
    non-whitespace char. If a non-whichspace char is followed
    by whitespace, then consider it part of a word. Once a word
    is found, we loop backwards starting at the index

data table

NAME               DESCRIPTION
line               parameter - the line of text to reverse.
num_words          variable - the word count.
prev_word          variable - index of the whitespace after
                              the last found word.
index              variable - index of the char being read.
backwards          variable - index moving backwards over
                              the found word.

*/
    int num_words = 0;
    int prev_word = 0;

    // Iterate over every char
    for (int index = 0; index < MAX_CHARS; index++) {
        /* If the current char is not whitespace, but the next char is,
           then we've reached the end of a word. */
        if (is_whitespace(line[index]) == 0 && is_whitespace(line[index + 1]) == 1 && index > 0) {
            // Write the word by moving backwards through the array
            for (int backwards = index; backwards >= prev_word; backwards--) {

                // Print chars in reverse order
                printf("%c", line[backwards]);
            }

            prev_word = index; // Record where the last word ended
            num_words++; // Increment number of words for this line
        }

        // Print non-newline whitespace in original position
        if (is_whitespace(line[index]) == 1 && line[index] != '\n' && line[index - 1] != '\n')
            printf("%c", line[index]);

        // Break early if all words were found
        if (line[index] == '\0' || line[index] == '\n')
            break;
    }
    return num_words;
}

int main(int argc, char *argv[]) {
/*  main is the controlling function of the program.
    main first prints the program info. Next, it initiailzes
    user_input and waits for the user to input text. Next it
    loops over each line of input, reverses each word and
    counts the number of words. Once that is complete, it
    counts the number of each letter and prints the stats
    for that line. Finally, it prints the stats for the
    entire input and exits.
    
main data table

NAME                DESCRIPTION
user_input          variable - user input separated by line.
lines               variable - the number of input lines.
total_words         variable - the total number of words.
total_letters       variable - the total count of each letter.
line                variable - the current input line being read.
num_words           variable - number of words in the current line.
num_letters         variable - number of each letter in the line.
                    
*/
    print_prog_info(NAME, DESC);

    char user_input[MAX_LINES][MAX_CHARS];
    int lines = get_input_lines(user_input);
    int total_words = 0;
    int *total_letters = malloc(26 * sizeof(int));

    printf("\n");

    for (int line = 0; line < lines; line++) {
        printf("%d: ", line + 1); // Print the line number before the line

        // Print lines in reverse with line stats
        int num_words = get_words_print_reverse(user_input[line]);
        int *num_letters = (int*) malloc(26 * sizeof(int));
        num_letters = count_letters(user_input[line]);
        printf("\n%d: ", line + 1); // Print the line number before stats
        print_stats(num_words, num_letters);

        // Update totals
        total_words += num_words;
        total_letters = add_int_arrays(total_letters, num_letters, 26);

        free(num_letters);
    }

    // Print stats for whole input
    printf("\nTotal: ");
    print_stats(total_words, total_letters);

    free(total_letters);
}
