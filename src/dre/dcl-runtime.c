#include <stdio.h>
#include <stdlib.h> // exit()
#include <unistd.h> // fork()
#include <sys/wait.h> // waitpid()
#include <string.h> // strstr()


#define BUF_SIZE 1000


int main() {

	// Infinite run loop
	pid_t pid;
	char filename[BUF_SIZE];
	for (;;) {

		// Clean up zombie processes
		while (waitpid((pid_t) -1, 0, WNOHANG) > 0);
		
		// Give name of file for lli to interpret
		printf("DCL file to execute: ");
		fflush(stdout);
		if (fgets(filename, BUF_SIZE, stdin) <= 0)
			continue;
		*strstr(filename, "\n") = '\0';

		// Execute file with lli
		if ((pid = fork()) < 0) {
			fprintf(stderr, "fork() failed\n");
			exit(1);
		} else if (pid == 0) { // Child process
			execlp("lli", "lli", filename, NULL);
			fprintf(stderr, "execl() failed\n");
			exit(1);
		} else { // Parent process
			
		}

	}

	return 0;
}
