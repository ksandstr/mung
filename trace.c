
#include <stdio.h>
#include <stdlib.h>
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


static struct trace_item *get_trace_item(int id)
{
	struct trace_item key = { .id = id };
	void *ptr = htable_get(&trace_status, hash_trace_item(&key, NULL),
		&cmp_trace_item_id, &key);
	return container_of(ptr, struct trace_item, id);
}


COLD void trace_enable(int id)
{
	trace_on = true;
	struct trace_item *it = get_trace_item(id);
	if(it == NULL) {
		it = malloc(sizeof(struct trace_item));
		it->id = id;
		it->enabled = 0;
		bool ok = htable_add(&trace_status, hash_trace_item(it, NULL), it);
		if(!ok) panic("allocation failure in htable_add (from trace.c)");
	}
	it->enabled++;
}


COLD void trace_disable(int id)
{
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
