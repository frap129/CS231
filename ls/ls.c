#include <dirent.h>
#include <stdio.h>
#include <string.h>

#define MAX_NUM_FILES 100
#define MAX_NAME_LEN  100

struct dirent *entry;
DIR *dir;

int open_dir(int argc, char *argv[]) {
    // Open either the current directory or the given one
    if (argc == 1)
    	dir = opendir(".");
    else
    	dir = opendir(argv[1]);

    // Check if directory exists
    if(dir == NULL) {
        printf("No such directory\n");
        return 1;
    }

    return 0;
}

int main(int argc, char *argv[]) {
    // Open requested directory
	if (open_dir(argc, argv))
	    return 1;

    // Add files in current directory to array
    char list[MAX_NUM_FILES][MAX_NAME_LEN], tmp[MAX_NAME_LEN];
    int count = 0;
    while ((entry = readdir(dir)) != NULL) {
        strcpy(list[count], entry->d_name);
        count++;
    }

    // Sort array alphabetically
    int i, j;
    for (i = 0; i <= count; i++)
      for (j = i + 1; j <= count; j++) {
         if (strcmp(list[i], list[j]) > 0) {
            strcpy(tmp, list[i]);
            strcpy(list[i], list[j]);
            strcpy(list[j], tmp);
         }
      }

    // Print file names
    for (i = 0; i <= count; i++)
        printf("%s ", list[i]);
    printf("\n");

    // Close directory pointer
    closedir(dir);
    return 0;
}
