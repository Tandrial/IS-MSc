#include <sys/ptrace.h>
#include <sys/reg.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>

int do_child(int argc, char **argv) {
  char *args [argc + 1];
  memcpy(args, argv, argc * sizeof(char*));
  args[argc] = NULL;
  // setup ptrace
  ptrace(PTRACE_TRACEME);
  // stop itself, so parent can see the execp syscall
  kill(getpid(), SIGSTOP);
  return execp(args[0], args);
}

int wait_for_syscall(pid_t child) {
  int status;
  while (1) {
    ptrace(PTRACE_SYSCALL, child, 0, 0);
    waitpid(child, &status, 0);
    if (WIFSTOPPED(status) && WSTOPSIG(status) & 0x8) return 0;
    if (WIFEXITED(status)) return 1;
  }
}

int do_trace(pid_t child) {
  int status, syscall, retval;
  waitpid(child, &status, 0);
  // We want to trace the child prcoess
  ptrace(PTRACE_SETOPTIONS, child, 0, PTRACE_O_TRACESYSGOOD);
  while (1) {
    // wait for syscall from child
    if (wait_for_syscall(child) != 0) break;
    // get number of syscall
    syscall = ptrace(PTRACE_PEEKUSER, child, sizeof(long) * ORIG_EAX);
    fprintf(stderr, "syscall(%d) = " , syscall);
    // wait for child syscall to finish
    if (wait_for_syscall(child) != 0) break;
    // get return value from syscall
    retval = ptrace(PTRACE_PEEKUSER, child, sizeof(long) * EAX);
    fprintf(stderr, "%d\n", retval);
  }
  return 0;
}

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s prog args\n", argv[0]);
        exit(1);
    }

    pid_t child = fork();
    if (child == 0) {
        return do_child(argc - 1, argv + 1);
    } else {
        return do_trace(child);
    }
}
