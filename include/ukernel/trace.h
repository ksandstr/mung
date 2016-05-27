
#ifndef SEEN_UKERNEL_TRACE_H
#define SEEN_UKERNEL_TRACE_H

#include <stdbool.h>
#include <ccan/likely/likely.h>


#ifndef NDEBUG
#include <stdio.h>
#include <ukernel/misc.h>
#define TRACE_MSG(id, fmt, ...) do { \
		if(unlikely(trace_is_enabled((id)))) { \
			if(trace_is_opt((id), TRACE_TIME_PREFIX)) { \
				printf("<%lu>", (unsigned long)read_global_timer()); \
			} \
			printf(fmt, ##__VA_ARGS__); \
		} \
	} while(0)
#else
#define TRACE_MSG(id, fmt, ...)
#endif


/* options */
#define TRACE_TIME_PREFIX 0

/* special "global option" item */
#define TRID_GLOBAL_OPT	0


extern void trace_enable(int id);
extern void trace_disable(int id);
extern bool trace_is_enabled(int id);
extern bool trace_is_opt(int id, int option);
extern void trace_set_opt(int id, int option, bool value);


/* all defined trace IDs. keep this in sync with the table in
 * exception.c:check_trace_control(), or else.
 */
enum trace_id {
	TRID_SCHED = 1,
	TRID_THREAD,
	TRID_MAPDB,
	TRID_IPC,
	TRID_IPC_REDIR,
	TRID_IRQ,
};

#endif
