#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>

void execute(char *input) {
	pid_t pid = fork();

	if (pid == 0) {
		system(input);
	}
}

void copy_input() {

    int character = 0;

    // Run until Ctrl+D (EOF) is sent
    while (character != EOF) {
        char *input = malloc(1);
        int length = 0;

        printf("[FakeShell] $ ");
        // Read each line of input char by char
        while((character = fgetc(stdin)) != '\n') {
            input[length++] = character;
            input = realloc(input, length + 1); // Increase memory as needed
        }

        execute(input);
        free(input);
    }
}

int main(int argc, char *argv[]) {
    copy_input();
}
