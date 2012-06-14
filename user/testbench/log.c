
/* simple deferred test logging.
 *
 * the point here is that printf(), being backed by a super naïve serial I/O
 * routine, tends to take up a whole lot of time while it's being executed.
 * things like the scheduling test tend to become less useful as milliseconds
 * are lost for gathering of data that might or might not have been useful, or
 * worse yet, cause test developers to write fewer useful tracing statements.
 *
 * atomicity is ensured with a threadswitch-backed spinlock. this works for
 * runnable threads of lower priority, and degenerates to a busyloop when no
 * other thread wants to run.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <ccan/list/list.h>

#include <l4/types.h>
#include <l4/ipc.h>
#include <l4/thread.h>
#include <l4/schedule.h>

#include "defs.h"


struct log_entry
{
	struct list_node link;
	int length;
	char str[];
};


static L4_ThreadId_t lock_tid = { .raw = 0 };
static struct list_head log_list = LIST_HEAD_INIT(log_list);


static void lock_module(void)
{
	L4_ThreadId_t self = L4_Myself();
	int spins = 0;
	while(!__sync_bool_compare_and_swap(&lock_tid.raw, 0, self.raw)) {
		L4_ThreadSwitch(lock_tid);

		/* let's be friendly, though. */
		if(++spins > 200000) {
			spins--;
			L4_Sleep(L4_TimePeriod(1 * 1000));
		}
	}
}


static void unlock_module(void)
{
	L4_ThreadId_t self = L4_Myself();
	assert(lock_tid.raw == self.raw);
	lock_tid.raw = 0;
	__sync_synchronize();
}


int log_f(const char *fmt, ...)
{
	lock_module();
	va_list al;
	va_start(al, fmt);
	int len = vsnprintf(NULL, 0, fmt, al);
	va_end(al);

	struct log_entry *ent = malloc(sizeof(struct log_entry)
		+ (len + 1) * sizeof(char));
	va_start(al, fmt);
	ent->length = vsnprintf(ent->str, len + 1, fmt, al);
	va_end(al);
	while(ent->length > 0 && ent->str[ent->length - 1] == '\n') {
		ent->str[--ent->length] = '\0';
	}

	list_add_tail(&log_list, &ent->link);
	len = ent->length;
	unlock_module();

	return len;
}


void flush_log(bool print)
{
	lock_module();

	struct log_entry *next, *child;
	list_for_each_safe(&log_list, child, next, link) {
		list_del(&child->link);
		if(print) printf("test log: %s\n", child->str);
		free(child);
	}
	assert(list_empty(&log_list));

	unlock_module();
}
