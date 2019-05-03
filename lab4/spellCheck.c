/* Joseph Maples
   CS-231 02L
   Assignment 4 - Spell check controller
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>

void lex_child(int pipe[2], char *file_name) {
/*  lex_child configures the pipes and arguments for
    the child process that runs lex, and executes
    lex.out.

data table

NAME               DESCRIPTION
pipe               parameter - the pipe to feed stdout to.
file_name          parameter - the value of argv[1], a file name.
args               variable - arguments for executing lex.out.

*/
    close(pipe[0]); // Close read end, sort reads this
    dup2(pipe[1], STDOUT_FILENO);
    close(pipe[1]);

    char *args[3] = {"./lex.out", file_name, NULL};
    execv(args[0], args);
}

void sort_child(int in_pipe[2], int out_pipe[2]) {
/*  sort_child configures the pipes and arguments for
    the child process that runs sort, and executes
    sort -f.

data table

NAME               DESCRIPTION
in_pipe            parameter - the pipe to feed to stdin.
out_pipe           parameter - the the pipe to feed stdout to.
args               variable - arguments for executing sort.

*/
    dup2(in_pipe[0], STDIN_FILENO);
    close(in_pipe[0]);

    close(out_pipe[0]); // Close read end, uniq reads this
    dup2(out_pipe[1], STDOUT_FILENO);
    close(out_pipe[1]);

    char *args[3] = {"sort", "-f", NULL};
    execvp(args[0], args);
}

void uniq_child(int in_pipe[2], int out_pipe[2]) {
/*  uniq_child configures the pipes and arguments for
    the child process that runs uniq, and executes
    uniq -i.

data table

NAME               DESCRIPTION
in_pipe            parameter - the pipe to feed to stdin.
out_pipe           parameter - the the pipe to feed stdout to.
args               variable - arguments for executing uniq.

*/
    dup2(in_pipe[0], STDIN_FILENO);
    close(in_pipe[0]);

    close(out_pipe[0]); // Close read end, compare reads this
    dup2(out_pipe[1], STDOUT_FILENO);
    close(out_pipe[1]);

    char *args[3] = {"uniq", "-i", NULL};
    execvp(args[0], args); 
}

void compare_child(int pipe[2], char *dict_name) {
/*  compare_child configures the pipes and arguments
    for the child process that runs compare, and
    executes compare.out.

data table

NAME               DESCRIPTION
pipe               parameter - the pipe to feed stdout to.
dict_name          parameter - the value of argv[2], the dictionary name.
args               variable - arguments for executing lex.out

*/
    dup2(pipe[0], STDIN_FILENO);
    close(pipe[0]);

    char *args[3] = {"./compare.out", dict_name, NULL};
    execv(args[0], args);
}

void write_log(int lex_pid, int sort_pid, int uniq_pid, int compare_pid) {
/*  write_log opens a file discriptor to a file named
    "spellCheck.log" and writes the name and PID of
    every child to the file in order of execution.

data table

NAME               DESCRIPTION
lex_pid            parameter - the PID of the lex child.
sort_pid           parameter - the PID of the sort child.
uniq_pid           parameter - the PID of the uniq child.
compare_pid        parameter - the PID of the compare child.
log                variable - file descriptor to spellCheck.log.

*/
    FILE *log = fopen("spellCheck.log", "w"); // Open log in write mode
    fprintf(log, "lex.out pid: %d\n", lex_pid);
    fprintf(log, "sort pid: %d\n", sort_pid);
    fprintf(log, "uniq pid: %d\n", uniq_pid);
    fprintf(log, "compare pid: %d\n", compare_pid);
    fclose(log);
}

void init_children(char *file, char *dict) {
/*  init_children first initializes a pipe that will
    connect the lex child to the sort child, then forks
    the lex child. The parent process then initializes
    a pipe that will connect the sort child to the uniq
    child, then fork the sort child. Next, the parent
    initializes a pipe that will connect the uniq child
    to the compare child and forks the uniq child.
    Finally, the parent forks the compare child and
    writes all child PIDs to the log.

data table

NAME               DESCRIPTION
file               parameter - the value of argv[1], a file name.
dict               parameter - the value of argv[2], the dictionary name.
lex_pid            parameter - the PID of the lex child.
sort_pid           parameter - the PID of the sort child.
uniq_pid           parameter - the PID of the uniq child.
compare_pid        parameter - the PID of the compare child.
lex2sort           variable - pipe from lex to sort.
sort2uniq          variable - pipe from sort to uniq.
uniq2compare       variable - prpe from uniq to compare.

*/
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
        close(lex2sort[1]); // Close write end, lex writes to this
        waitpid(lex_pid, NULL, 0);
        pipe(sort2uniq);
        sort_pid = fork();

        if (sort_pid == 0) {
            sort_child(lex2sort, sort2uniq);
        } else {
            close(sort2uniq[1]); // Close write end, sort writes to this
            waitpid(sort_pid, NULL, 0);
            pipe(uniq2compare);
            uniq_pid = fork();

            if (uniq_pid == 0) {
                uniq_child(sort2uniq, uniq2compare);
            } else {
                close(uniq2compare[1]); // Close write end, uniq writes to this
                waitpid(uniq_pid, NULL, 0);
                compare_pid = fork();

                if(compare_pid == 0) {
                    compare_child(uniq2compare, dict);
                } else {
                    waitpid(compare_pid, NULL, 0);

                    // Close read ends, only used by child processes
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
/*  main first checks that the correct number of
    arguments were supplied to the program, and
    will exit if too few or too many were given.
    If 2 args were specified, they get assigned to
    strings and passed to init_children.

data table

NAME               DESCRIPTION
argc               parameter - the number of args supplied.
argv               parameter - the arguments supplied.
input              parameter - the value of argv[1], a file name.
dictionary         parameter - the value of argv[2], the dictionary name.

*/
    // Verify arguments
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
