/* Joseph Maples
   CS-231 02L
   Assignment 3 - Cloning cat
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define B_INDEX 0
#define E_INDEX 1
#define N_INDEX 2

int print_file(char *prog_name, char *file_name, int *switches, int line_num) {
/*  print_file opens the givien file name in read mode, and
    passes the contents to stdout one char at a time. While
    outputting, it looks for newline characters so that the
    enabled swithes can be handled.

 data table

 NAME               DESCRIPTION
 prog_name          parameter - argv[0], the program name.
 file_name          parameter - the file to read and output.
 switches           parameter - the array of switch values.
 line_num           parameter - the current line number.
 file_pointer       variable - pointer to the data contained in
                               the file located at file_name.
 ending             variable - the line ending, may be "$\n" or
                               "\n" depending on the E switch.
 cur_char           variable - the current character being read.
 prev_char          variable - the character before cur_char.

 */
    FILE *file_pointer = fopen(file_name, "r"); // Open file in read mode

    // Handle non-existent file
    if (file_pointer == NULL) {
       fprintf(stderr, "%s: %s: No such file for directory\n", prog_name, file_name);
       return line_num;
    }

    char *ending = switches[E_INDEX] ? "$\n" : "\n"; // Set line ending
    char cur_char;
    char prev_char = '\0';

    // Read file until EOF is reached
    while((cur_char = fgetc(file_pointer)) != EOF) {
        // Determine if line number should be printed
        if ((switches[N_INDEX] || switches[B_INDEX]) &&
            (prev_char == '\n' || prev_char == '\0')) {
            // Determine if empty lines need numbers
            if (!switches[B_INDEX] || cur_char != '\n')
                printf("%6d\t", line_num++); // Print line number
        }

        // Print each char until we reach end of the line
        if (cur_char != '\n')
            printf("%c", cur_char);
        else
            printf("%s", ending); // Print line ending

        prev_char = cur_char;
    }
    fclose(file_pointer);
    return line_num;
}

int copy_input(int *switches, int line_num) {
/*  copy_input waits for a newline, and stores everything
    that was input before the newline. After looking at the
    values of the switches, it will format the output to
    have the line number if b or n was set, and end with $
    if E was set. Finally, it prints the line.

 data table

 NAME               DESCRIPTION
 switches           parameter - the array of switch values.
 character          variable - the most recent character that
                               was input.
 line_num           parameter - the number of lines input so far.
 input              variable - the line of input so far.
 length             variable - the length of the input so far.
 ending             variable - the line ending, may be "$\n" or
                               "\n" depending on the E switch.

 */
    int character = 0;

    // Run until Ctrl+D (EOF) is sent
    while (character != EOF) {
        char *input = malloc(1);
        int length = 0;

        // Read each line of input char by char
        while((character = fgetc(stdin)) != '\n' && character != EOF) {
            input[length++] = character;
            input = realloc(input, length + 1); // Increase memory as needed
        }

        clearerr(stdin); // Clear error after reaching EOF

        // If EOF is reached on a blank line, exit early
        if (length == 0 && character == EOF)
            return line_num;

        input[length] = '\0';

        char *ending = switches[E_INDEX] ? "$\n" : "\n"; // Set line ending

        // Determine if line number should be printed
        if (switches[N_INDEX] || (switches[B_INDEX] && strlen(input) >= 1)) {
            printf("%6d\t%s%s", line_num++, input, ending); // Print line number
        } else
            printf("%s%s", input, ending);

        free(input);
    }

    return line_num;
}

int main(int argc, char *argv[]) {
/*  main is the controlling function of the program.
    First, main initializes switches and input size. Next,
    it assigns the size of argv to input_size. This is
    likely excess, should the user add non-input arguments
    like "-E", but using a few extra bytes isn't an issue.
    Next, it loops over each argument, determining whether
    it is a switch or a source of input. If it's a switch,
    the corresponding switch value in switches is set to 1.
    If it's input, its added into the list of inputs and
    num_inputs is incremented. Finally, each source of
    input is read and output.

 data table

 NAME               DESCRIPTION
 switches           variable - the array of switch values.
 input_size         variable - the size to allocate for 'inputs'.
 arg                variable - the current argument being read.
 inputs             variable - the list of input sources.
 num_inputs         variable - the number of inputs.
 arg_len            variable - the length of the argument.
 chars              variable - index of the char being read.
 line               variable - the running total number of lines.
 input              variable - index of inputs being read.

 */
    int *switches = malloc(3 * sizeof(int));

    int input_size = 0;

    // Get input size based on argv at each arg
    for (int arg = 1; arg < argc; arg++)
        input_size += sizeof(argv[arg]);

    char **inputs = malloc(input_size);
    int num_inputs = 0;

    // Determine whether each argument is a switch or input
    for (int arg = 1; arg < argc; arg++) {
        if (argv[arg][0] == '-' && strlen(argv[arg]) > 1) {
            int arg_len = strlen(argv[arg]);
            // Determine which switch was used
            for (int chars = 1; chars < arg_len; chars++) {
                if (argv[arg][chars] == 'b')
                    switches[B_INDEX] = 1;
                else if (argv[arg][chars] == 'E')
                    switches[E_INDEX] = 1;
                else if (argv[arg][chars] == 'n')
                    switches[N_INDEX] = 1;
            }
        } else {
            inputs[num_inputs++] = argv[arg];
        }
    }

    int line = 1;
    // If no inputs, only handle stdin
    if (num_inputs == 0)
        copy_input(switches, line);
    else
        // Get each input, one source at a time
        for (int input = 0; input < num_inputs; input++) {
            if (!strcmp(inputs[input], "-"))
                line = copy_input(switches, line);
            else
                line = print_file(argv[0], inputs[input], switches, line);
        }

    free(switches);
    free(inputs);
    return 0;
}
