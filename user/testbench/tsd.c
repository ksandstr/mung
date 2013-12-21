
#include <stdlib.h>
#include <ccan/htable/htable.h>

#include <l4/thread.h>
#include <ukernel/util.h>

#include "defs.h"


struct tsd_key
{
	int key;
	void (*destructor)(void *ptr);
};


struct tsd_item {
	int key;
	void *ptr;
};


struct tsd
{
	struct htable tsd_vals;
};


static size_t hash_tsd_key(const void *key, void *priv);


static int next_tsd_key = 1;
static struct htable tsd_keys = HTABLE_INITIALIZER(tsd_keys,
	&hash_tsd_key, NULL);


/* used for tsd_item
 * TODO: rename to match
 */
static size_t hash_int(const void *k, void *priv) {
	return int_hash(*(const int *)k);
}


static size_t hash_tsd_key(const void *key, void *priv) {
	const struct tsd_key *k = key;
	return int_hash(k->key);
}


void tsd_key_create(int *key_p, void (*destructor)(void *ptr))
{
	*key_p = next_tsd_key++;
	struct tsd_key *k = malloc(sizeof(struct tsd_key));
	k->key = *key_p;
	k->destructor = destructor;		/* TODO: use! */
	htable_add(&tsd_keys, hash_tsd_key(k, NULL), k);
}


void tsd_set(int key, void *ptr)
{
	if(key == 0) return;

	struct tsd *tsd = (void *)L4_UserDefinedHandle();
	if(tsd == NULL) {
		tsd = malloc(sizeof(struct tsd));
		htable_init(&tsd->tsd_vals, &hash_int, NULL);
		L4_Set_UserDefinedHandle((L4_Word_t)tsd);
	}

	size_t hv = int_hash(key);
	struct tsd_item *val = htable_get(&tsd->tsd_vals, hv, &int_eq, &key);
	if(val == NULL) {
		val = malloc(sizeof(struct tsd_item));
		val->key = key;
		val->ptr = NULL;
		htable_add(&tsd->tsd_vals, hv, val);
	}
	val->ptr = ptr;
}


void *tsd_get(int key)
{
	if(key == 0) return NULL;

	struct tsd *tsd = (void *)L4_UserDefinedHandle();
	if(tsd == NULL) return NULL;

	struct tsd_item *val = htable_get(&tsd->tsd_vals, int_hash(key),
		&int_eq, &key);
	return val != NULL ? val->ptr : NULL;
}


void tsd_clear(void)
{
	/* toss the TSD values. */
	struct tsd *tsd = (void *)L4_UserDefinedHandle();
	if(tsd != NULL) {
		struct htable_iter it;
		for(struct tsd_item *val = htable_first(&tsd->tsd_vals, &it);
			val != NULL;
			val = htable_next(&tsd->tsd_vals, &it))
		{
			struct tsd_key *key = htable_get(&tsd_keys,
				int_hash(val->key), &int_eq, &val->key);
			if(key != NULL && key->destructor != NULL) {
				(*key->destructor)(val->ptr);
			}
			htable_delval(&tsd->tsd_vals, &it);
			free(val);
		}
		free(tsd);
		L4_Set_UserDefinedHandle(0);
	}
}
