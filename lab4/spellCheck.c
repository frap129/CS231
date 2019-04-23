/* Joseph Maples
   CS-231 02L
   Assignment 4 - Spell check controller
 */

#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {
    if (argc < 3) {
        fprintf(stderr, "%s: Less than two files supplied\n", argv[0]);
        exit(1);
    } else if (argc > 3) {
        fprintf(stderr, "%s: More than two files supplied\n", argv[0]);
        exit(1);
    }
    char *input = argv[1];
    char *dictionary = argv[2];
    return 0;
}