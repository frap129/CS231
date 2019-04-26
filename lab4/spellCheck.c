/* Joseph Maples
   CS-231 02L
   Assignment 4 - Spell check controller
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>

void null_string(char *string, int size) {
    for (int i = 0; i < size; i++)
        string[i] = '\0';
}

void lex_child(int pipe[2], char *file_name) {
    close(pipe[0]); //close read end, sort reads this
    dup2(pipe[1], STDOUT_FILENO);
    close(pipe[1]);

    char *args[3];
    args[0] = "./lex.out";
    args[1] = file_name;
    args[2] = NULL;
    execv(args[0], args);
}

void sort_child(int in_pipe[2], int out_pipe[2]) {
    close(in_pipe[1]);//close write end, lex writes this
    dup2(in_pipe[0], STDIN_FILENO);
    close(in_pipe[0]);

    close(out_pipe[0]); //close read end, uniq reads this
    dup2(out_pipe[1], STDOUT_FILENO);
    close(out_pipe[1]);

    char *args[3];
    args[0] = "sort";
    args[1] = "-f"; // Ignore case
    args[2] = NULL;
    execvp(args[0], args); 
}

void uniq_child(int in_pipe[2], int out_pipe[2]) {
    close(in_pipe[1]);//close write end, sort writes this
    dup2(in_pipe[0], STDIN_FILENO);
    close(in_pipe[0]);

    close(out_pipe[0]); //close read end, compare reads this
    dup2(out_pipe[1], STDOUT_FILENO);
    close(out_pipe[1]);

    char *args[3];
    args[0] = "uniq";
    args[1] = "-i"; // Ignore case
    args[2] = NULL;
    execvp(args[0], args); 
}

int init_children(char *file, char *dict) {
    pid_t lex_pid;
    pid_t sort_pid;
    pid_t uniq_pid;
    int lex[2];
    int sort[2];
    int uniq[2];

    pipe(lex);
    lex_pid = fork();

    if (lex_pid == 0) {
        lex_child(lex, file);
        return 0;
    } else {
        close(lex[1]);
        waitpid(lex_pid, NULL, 0);
        pipe(sort);
        sort_pid = fork();
        if (sort_pid == 0) {
            sort_child(lex, sort);
            return 0;
        } else {
            close(sort[1]);
            waitpid(sort_pid, NULL, 0);
            pipe(uniq);
            uniq_pid = fork();
            if (uniq_pid == 0) {
                uniq_child(sort, uniq);
                return 0;
            } else {
                close(uniq[1]);
                waitpid(uniq_pid, NULL, 0);
                // Prints the output of sort for debugging
                char *msg = malloc(1024);
                null_string(msg, 1024);
                read(uniq[0], msg, 1024);
                printf("%s", msg);
                close(lex[0]);
                close(sort[0]);
                close(uniq[0]);
                int signal;
                return 0;
            }
        }
    }
}

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
    init_children(input, dictionary);
    return 0;
}