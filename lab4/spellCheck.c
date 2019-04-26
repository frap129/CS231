/* Joseph Maples
   CS-231 02L
   Assignment 4 - Spell check controller
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>

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

void compare_child(int pipe[2], char *dict_name) {
    close(pipe[1]); //close write end, uniq writes this
    dup2(pipe[0], STDIN_FILENO);
    close(pipe[0]);

    char *args[3];
    args[0] = "./compare.out";
    args[1] = dict_name;
    args[2] = NULL;
    execv(args[0], args);
}

void write_log(int lex_pid, int sort_pid, int uniq_pid, int compare_pid) {
    FILE *log = fopen("spellCheck.log", "w"); // Open log in write mode
    fprintf(log, "lex.out pid: %d\n", lex_pid);
    fprintf(log, "sort pid: %d\n", sort_pid);
    fprintf(log, "uniq pid: %d\n", uniq_pid);
    fprintf(log, "compare pid: %d\n", compare_pid);
    fclose(log);
}

void init_children(char *file, char *dict) {
    pid_t lex_pid;
    pid_t sort_pid;
    pid_t uniq_pid;
    pid_t compare_pid;
    int lex2sort[2];
    int sort2uniq[2];
    int uniq2compare[2];

    pipe(lex2sort);
    lex_pid = fork();

    if (lex_pid == 0) {
        lex_child(lex2sort, file);
    } else {
        close(lex2sort[1]);
        waitpid(lex_pid, NULL, 0);
        pipe(sort2uniq);
        sort_pid = fork();
        if (sort_pid == 0) {
            sort_child(lex2sort, sort2uniq);
        } else {
            close(sort2uniq[1]);
            waitpid(sort_pid, NULL, 0);
            pipe(uniq2compare);
            uniq_pid = fork();
            if (uniq_pid == 0) {
                uniq_child(sort2uniq, uniq2compare);
            } else {
                close(uniq2compare[1]);
                waitpid(uniq_pid, NULL, 0);
                compare_pid = fork();
                if(compare_pid == 0) {
                    compare_child(uniq2compare, dict);
                } else {
                    waitpid(compare_pid, NULL, 0);
                    close(lex2sort[0]);
                    close(sort2uniq[0]);
                    close(uniq2compare[0]);
                    write_log(lex_pid, sort_pid, uniq_pid, compare_pid);
                }
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