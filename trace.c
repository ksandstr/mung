
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <ccan/container_of/container_of.h>
#include <ccan/compiler/compiler.h>

#include <ukernel/util.h>
#include <ukernel/misc.h>
#include <ukernel/trace.h>


struct trace_item
{
	int16_t enabled;
	uint16_t opts;
};


static bool trace_on = false;
static struct trace_item *trace_status = NULL;
static size_t trace_status_size = 0;


static struct trace_item *add_trace_item(int id)
{
	if(id >= trace_status_size) {
		assert(id >= 0);
		struct trace_item *repl = realloc(trace_status,
			(id + 1) * sizeof(struct trace_item));
		if(repl == NULL) {
			panic("can't reallocate trace_status");
		}
		for(int i=trace_status_size; i <= id + 1; i++) {
			repl[i] = (struct trace_item){ };
		}
		trace_status = repl;
		trace_status_size = id + 1;
	}

	return &trace_status[id];
}


static struct trace_item *get_trace_item(int id)
{
	assert(id >= 0);
	return id < trace_status_size ? &trace_status[id] : NULL;
}


COLD void trace_enable(int id)
{
	trace_on = true;
	struct trace_item *it = get_trace_item(id);
	if(it == NULL) it = add_trace_item(id);
	it->enabled++;
}


COLD void trace_disable(int id)
{
	assert(id != TRID_GLOBAL_OPT);
	struct trace_item *it = get_trace_item(id);
	if(it != NULL) {
		assert(it->enabled > 0);
		it->enabled--;
	}
}


bool trace_is_enabled(int id)
{
	if(!trace_on) return false;
	struct trace_item *it = get_trace_item(id);
	return it != NULL && it->enabled > 0;
}


bool trace_is_opt(int id, int option)
{
	struct trace_item *it = get_trace_item(id),
		*global = get_trace_item(TRID_GLOBAL_OPT);
	return (it != NULL && CHECK_FLAG(it->opts, 1 << option))
		|| (global != NULL && CHECK_FLAG(global->opts, 1 << option));
}


COLD void trace_set_opt(int id, int option, bool value)
{
	struct trace_item *it = get_trace_item(id);
	if(it == NULL) return;
	if(value) it->opts |= 1 << option; else it->opts &= ~(1 << option);
}
