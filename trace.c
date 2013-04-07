
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <ccan/htable/htable.h>
#include <ccan/container_of/container_of.h>
#include <ccan/compiler/compiler.h>

#include <ukernel/util.h>
#include <ukernel/misc.h>
#include <ukernel/trace.h>


struct trace_item
{
	int id;
	int enabled;
	uint8_t opts;
};


static size_t hash_trace_item(const void *, void *);


static bool trace_on = false;
static struct htable trace_status = HTABLE_INITIALIZER(trace_status,
	&hash_trace_item, NULL);


static size_t hash_trace_item(const void *key, void *priv) {
	const struct trace_item *it = key;
	return int_hash(it->id);
}


static bool cmp_trace_item_id(const void *a, void *b) {
	const struct trace_item *c = a, *d = b;
	return c->id == d->id;
}


static struct trace_item *add_trace_item(int id)
{
	/* (could allocate them from a slab. but why?) */
	struct trace_item *it = malloc(sizeof(struct trace_item));
	if(it == NULL) panic("can't allocate trace_item");
	*it = (struct trace_item){ .id = id, .enabled = 0, .opts = 0 };
	bool ok = htable_add(&trace_status, hash_trace_item(it, NULL), it);
	if(!ok) panic("allocation failure in htable_add (from trace.c)");
	return it;
}


static struct trace_item *get_trace_item(int id)
{
	struct trace_item key = { .id = id };
	void *ptr = htable_get(&trace_status, hash_trace_item(&key, NULL),
		&cmp_trace_item_id, &key);
	if(ptr == NULL && id == TRID_GLOBAL_OPT) {
		/* create it. */
		return add_trace_item(id);
	}
	return ptr != NULL ? container_of(ptr, struct trace_item, id) : NULL;
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
