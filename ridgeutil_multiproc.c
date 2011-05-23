/*
 * Surrey Space Centre ridge tools for SAR data processing
 * Copyright (C) 2011  Peter Brett <p.brett@surrey.ac.uk>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "config.h"

#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/mman.h>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

#ifdef HAVE_VALGRIND_MEMCHECK_H
#  include <valgrind/memcheck.h>
#else
#  define VALGRIND_MAKE_MEM_UNDEFINED(ptr,len)
#  define VALGRIND_MAKE_MEM_NOACCESS(ptr,len)
#endif /* HAVE_VALGRIND_MEMCHECK_H */

#include "ridgeutil.h"

int rut_multiproc_threads = 1;
int rut_multiproc_in_child = 0;

int
rut_multiproc_task (void (*func)(int, int, void *), void *user_data)
{
  pid_t *child_pids;
  int i;
  int success = 1;

  assert (func);

  /* Short-circuit the single-threaded case. If this is already a
   * child process, force single thread. */
  if (rut_multiproc_threads < 2 || rut_multiproc_in_child) {
    func (0, 1, user_data);
    return success;
  }

  child_pids = malloc (rut_multiproc_threads * sizeof (pid_t));

  /* Fork child processes */
  for (i = 0; i < rut_multiproc_threads; i++) {
    pid_t child = fork();

    /* If this is the child process, run the function and exit */
    if (!child) {
      rut_multiproc_in_child = 1;
      func (i, rut_multiproc_threads, user_data);
      exit (0);
    }

    /* Otherwise, add pid to list of child pids. */
    child_pids[i] = child;
  }

  /* Now wait for child processes */
  for  (i = 0; i < rut_multiproc_threads; i++) {
    int status;
    waitpid (child_pids[i], &status, 0);
    if (!WIFEXITED(status) || WEXITSTATUS(status)) {
      fprintf (stderr, "Child process %i did not exit normally\n",
               child_pids[i]);
      success = 0;
    }
  }

  free (child_pids);
  return success;
}

void *
rut_multiproc_malloc (size_t size)
{
  size_t len = size + sizeof (size_t);
  size_t *mptr = mmap (NULL, len, PROT_READ | PROT_WRITE,
                       MAP_SHARED | MAP_ANONYMOUS, 0, 0);
  if (mptr == MAP_FAILED) return NULL;
  VALGRIND_MAKE_MEM_UNDEFINED (mptr, len);
  return (void *) (mptr + 1);
}

void
rut_multiproc_free (void *ptr)
{
  if (ptr == NULL) return;
  size_t *mptr = ((size_t *) ptr) - 1;
  size_t len = *mptr;
  munmap (mptr, len);
  VALGRIND_MAKE_MEM_NOACCESS (ptr, len);
}
