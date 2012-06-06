
#ifndef SEEN_UKERNEL_TRACE_H
#define SEEN_UKERNEL_TRACE_H

#include <stdbool.h>
#include <ccan/likely/likely.h>


#ifndef NDEBUG
#define TRACE_MSG(id, fmt, ...) do { \
		if(unlikely(trace_is_enabled((id)))) { \
			printf(fmt, __VA_ARGS__); \
		} \
	} while(0)
#else
#define TRACE_MSG(id, fmt, ...)
#endif


extern void trace_enable(int id);
extern void trace_disable(int id);
extern bool trace_is_enabled(int id);


/* directory of trace IDs */

enum trace_id {
	TRID_SCHED = 1,		/* everything in sched.c */
	TRID_THREAD,		/* everything in thread.c */
	TRID_MAPDB,
	TRID_IPC,
};

#endif
