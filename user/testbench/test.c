/* unit testing framework modelled after Check. */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <ctype.h>
#include <assert.h>

#include <ccan/list/list.h>
#include <ccan/container_of/container_of.h>
#include <ccan/htable/htable.h>
#include <ccan/hash/hash.h>
#include <ccan/str/str.h>
#include <ccan/strset/strset.h>
#include <ccan/talloc/talloc.h>

#include <l4/types.h>
#include <l4/thread.h>
#include <l4/ipc.h>

#include <ukernel/util.h>

#include "defs.h"
#include "test.h"


#define IN_TEST_MAGIC 0x51deb00b	/* with fuckings to the poh-leece */

#define E_FIXTURE_SETUP 1
#define E_FIXTURE_TEARDOWN 2


struct SRunner
{
	struct list_head suites;
	struct htable test_by_path, test_by_id;
};


struct Suite
{
	struct list_node runner_link;
	struct list_head cases;
	char name[];
};


struct TCase
{
	struct list_node suite_link;
	struct list_head tests, u_fixtures, c_fixtures;
	bool do_fork;
	char name[];
};


struct fixture
{
	struct list_node link;	/* in u_fixtures or c_fixtures */
	SFun setup, teardown;
};


struct test
{
	struct list_node tcase_link;
	void (*t_fun)(int);
	int low, high;
	char name[];
};


struct test_status {
	bool rc;		/* run_test() return value */
};


/* in srunner->test_* */
struct test_entry {
	Suite *s;
	TCase *tc;
	struct test *test;
	char path[];
};


static int in_test_key = 0;		/* ptr value, IN_TEST_MAGIC */


static size_t rehash_test_entry(const void *ptr, void *priv) {
	const struct test_entry *ent = ptr;
	return hash_string(ent->path);
}


static bool test_entry_str_compare(const void *cand, void *keyptr) {
	const struct test_entry *ent = cand;
	return streq(ent->path, (const char *)keyptr);
}


static struct test_entry *get_test_entry(struct htable *ht, const char *key)
{
	return htable_get(ht, hash_string(key),
		&test_entry_str_compare, key);
}


static int __attribute__((pure)) list_length(struct list_head *list)
{
	int count = 0;
	for(struct list_node *n = list->n.next; n != &list->n; n = n->next) {
		count++;
	}
	return count;
}


Suite *suite_create(const char *name)
{
	tsd_key_create(&in_test_key, NULL);

	int len = strlen(name);
	Suite *s = malloc(sizeof(Suite) + len + 1);
	list_head_init(&s->cases);
	memcpy(&s->name, name, len + 1);
	return s;
}


void suite_add_tcase(Suite *s, TCase *tc) {
	list_add_tail(&s->cases, &tc->suite_link);
}


TCase *tcase_create(const char *name)
{
	int len = strlen(name);
	TCase *tc = malloc(sizeof(TCase) + len + 1);
	*tc = (TCase){ .do_fork = true };
	list_head_init(&tc->tests);
	list_head_init(&tc->c_fixtures);
	list_head_init(&tc->u_fixtures);
	memcpy(tc->name, name, len + 1);
	return tc;
}


static void add_fixture(struct list_head *list, SFun setup, SFun teardown)
{
	struct fixture *f = malloc(sizeof(*f));
	*f = (struct fixture){ .setup = setup, .teardown = teardown };
	list_add_tail(list, &f->link);
}


void tcase_add_unchecked_fixture(TCase *tc, SFun setup, SFun teardown) {
	add_fixture(&tc->u_fixtures, setup, teardown);
}


void tcase_add_checked_fixture(TCase *tc, SFun setup, SFun teardown) {
	add_fixture(&tc->c_fixtures, setup, teardown);
}


void tcase_set_fork(TCase *tc, bool do_fork) {
	tc->do_fork = do_fork;
}


void tcase_add_test_info(TCase *tc, const struct test_info *info)
{
	int len = strlen(info->name);
	struct test *t = malloc(sizeof(struct test) + len + 1);
	*t = (struct test){
		.t_fun = info->test_fn,
		.low = info->iter_low, .high = info->iter_high,
	};
	memcpy(t->name, info->name, len + 1);
	list_add_tail(&tc->tests, &t->tcase_link);
}


struct test_thread_param
{
	TCase *tc;
	struct test *t;
	int val;
	L4_ThreadId_t end_tid;
};


/* non-local exits can happen through exit_thread(). */
static void test_wrapper_fn(void *param_ptr)
{
	const struct test_thread_param *p = param_ptr;
	tsd_set(in_test_key, (void *)IN_TEST_MAGIC);
	(*p->t->t_fun)(p->val);
	/* TODO: fill in a test_status report, return it through exit_thread() */

	exit_thread(NULL);
}


void exit_on_fail(void)
{
	struct test_status *st = malloc(sizeof(*st));
	if(st != NULL) {
		*st = (struct test_status){ .rc = false };
	}

	/* TODO: track and forcequit accessory threads as well. right now ones
	 * that aren't controlled by fixtures will be left hanging, which may
	 * compromise the test process under !do_fork.
	 */
	exit_thread(st);
}


bool in_test(void) {
	return tsd_get(in_test_key) == (void *)IN_TEST_MAGIC;
}


struct fixture_param {
	SFun fn;
	L4_ThreadId_t sync_tid;
};


static void fixture_wrapper_fn(void *param_ptr) {
	struct fixture_param *p = param_ptr;
	(*p->fn)();
}


static bool run_fixture_list(struct list_head *list, bool teardown)
{
	const L4_Time_t max_fixture_time = L4_TimePeriod(250 * 1000);
	bool rc = true;
	struct fixture *f;
	list_for_each(list, f, link) {
		struct fixture_param *p = malloc(sizeof(*p));
		*p = (struct fixture_param){
			.fn = teardown ? f->teardown : f->setup,
			.sync_tid = L4_Myself(),
		};
		L4_ThreadId_t thread = start_thread(&fixture_wrapper_fn, p);
		if(L4_IsNilThread(thread)) {
			printf("*** %s: start_thread() failed\n", __func__);
			free(p);
			return false;
		}

		L4_Word_t ec = 0;
		struct test_status *status = join_thread_long(thread,
			max_fixture_time, &ec);
		if(status == NULL && ec != 0) {
			printf("*** error: join of fixture thread failed, ec %#lx\n", ec);
			abort();
		} else {
			if(status != NULL) rc = status->rc;
			free(status);
			free(p);
		}

		if(!rc) break;
	}

	return rc;
}


static bool run_test(TCase *tc, struct test *t, int test_value)
{
	bool rc = true;

	struct test_thread_param *param = malloc(sizeof(*param));
	*param = (struct test_thread_param){
		.tc = tc, .t = t, .val = test_value, .end_tid = L4_Myself(),
	};
	L4_ThreadId_t thread = start_thread(&test_wrapper_fn, param);
	if(L4_IsNilThread(thread)) {
		printf("*** %s: start_thread() failed\n", __func__);
		rc = false;
		goto end;
	}

	/* join it. there's no concurrency here; threads are merely used as a
	 * nonlocal exit mechanism.
	 *
	 * FIXME: change the 3s timeout to a fancier model of how tests can be
	 * run, concurrently or not, modifiable with parameters per TCase.
	 */
	L4_Word_t ec = 0;
	struct test_status *status = join_thread_long(thread,
		L4_TimePeriod(3000 * 1000), &ec);
	if(status == NULL && ec != 0) {
		printf("*** %s: wrapper join failed, ec=%#lx\n", __func__, ec);
		rc = false;
	} else if(status != NULL && ec != 0) {
		printf("*** test caused a segmentation fault at %p\n", status);
		rc = false;
	} else if(status != NULL) {
		/* ordinary completion. */
		rc = status->rc;
		free(status);
	} else {
		/* non-notable completion. (FIXME: combine w/ above in
		 * test_wrapper_fn())
		 */
		assert(status == NULL && ec == 0);
	}

end:
	free(param);
	return rc;
}


SRunner *srunner_create(Suite *first_suite)
{
	SRunner *sr = malloc(sizeof(SRunner));
	list_head_init(&sr->suites);
	htable_init(&sr->test_by_path, &rehash_test_entry, NULL);
	htable_init(&sr->test_by_id, &rehash_test_entry, NULL);

	if(first_suite != NULL) srunner_add_suite(sr, first_suite);

	return sr;
}


void srunner_add_suite(SRunner *run, Suite *s)
{
	list_add_tail(&run->suites, &s->runner_link);

	TCase *tc;
	list_for_each(&s->cases, tc, suite_link) {
		struct test *t;
		list_for_each(&tc->tests, t, tcase_link) {
			char path[256];
			int pchars = snprintf(path, sizeof(path), "%s:%s:%s", s->name,
				tc->name, t->name);
			struct test_entry *ent = malloc(sizeof(struct test_entry)
				 + pchars + 1);
			ent->test = t;
			ent->tc = tc;
			ent->s = s;
			memcpy(ent->path, path, pchars + 1);
			assert(get_test_entry(&run->test_by_path, ent->path) == NULL);
			htable_add(&run->test_by_path, hash_string(ent->path), ent);
			assert(get_test_entry(&run->test_by_path, ent->path) == ent);
		}
	}
}


static int cmp_strptr(const void *ap, const void *bp) {
	const char *a = *(const char **)ap, *b = *(const char **)bp;
	return strcmp(a, b);
}


/* compute longest common name prefix within @list, extracted per
 * @link_off and @name_off.
 */
static size_t prefix_length(
	struct list_head *list, size_t link_off, size_t name_off)
{
	size_t num_names = list_length(list);
	char *names[num_names];
	int pos = 0;
	void *ptr;
	list_for_each_off(list, ptr, link_off) {
		names[pos++] = ptr + name_off;
	}
	qsort(names, num_names, sizeof(char *), &cmp_strptr);

	size_t max_prefix = 0;
	for(size_t i=1; i < num_names; i++) {
		const char *prev = names[i - 1], *cur = names[i];
		size_t p = 0;
		while(prev[p] == cur[p] && prev[p] != '\0') p++;
#if 0
		if(p > max_prefix) {
			printf("%s: between `%s' and `%s', p=%u, max_prefix=%u\n",
				__func__, prev, cur,
				(unsigned)p, (unsigned)max_prefix);
		}
#endif
		max_prefix = MAX(size_t, p, max_prefix);
	}

	return max_prefix;
}


/* grab up to @take - [# of digits in @iter if > 0] alpha characters from
 * @name, capitalize the first letter, and add @iter converted to a base-10
 * number (of at most 2 digits). the @iter goes on top of the last two if
 * the result would otherwise be longer than 4 characters.
 */
static void make_padded_name(
	char *out, const char *name, size_t take, size_t iter)
{
	char tmp[take + 1];
	int t = 0;
	for(int i = 0; name[i] != '\0' && t < take; i++) {
		if(isalpha(name[i])) tmp[t++] = name[i];
	}
	tmp[t] = '\0';
	if(iter == 0) {
		memcpy(out, tmp, t + 1);
	} else if(iter <= 10) {
		if(take > 3) tmp[--t] = '\0';
		snprintf(out, take + 2, "%s%d", tmp, iter - 1);
	} else if(iter <= 100) {
		if(take > 3) tmp[--t] = '\0';
		if(take > 2) tmp[--t] = '\0';
		snprintf(out, take + 3, "%s%02d", tmp, iter - 1);
	} else {
		assert(iter <= 100);
	}
	out[0] = toupper(out[0]);
}


/* this used to be objectively the worst thing i'd ever written. now it's
 * slightly better. i sincerely hope it's as painful to read as it was to
 * write. -ks
 */
static void add_test_ids(struct htable *table, SRunner *sr)
{
	void *setctx = talloc_new(NULL);
	struct strset suite_set, id_set;
	strset_init(&suite_set);
	strset_init(&id_set);	/* they're global. */
	size_t suite_pfx = prefix_length(&sr->suites,
		offsetof(Suite, runner_link), offsetof(Suite, name));
	Suite *s;
	list_for_each(&sr->suites, s, runner_link) {
		char suitename[32];
		for(int i = 0; i <= 100; i++) {
			make_padded_name(suitename, s->name,
				MIN(size_t, suite_pfx, 4) + 1, i);
			if(strset_get(&suite_set, suitename) == NULL) {
				strset_add(&suite_set, talloc_strdup(setctx, suitename));
				break;
			}
		}

		struct strset tcase_set;
		strset_init(&tcase_set);
		void *tcctx = talloc_new(setctx);
		size_t tcase_pfx = prefix_length(&s->cases,
			offsetof(TCase, suite_link), offsetof(TCase, name));
		TCase *tc;
		list_for_each(&s->cases, tc, suite_link) {
			char tcasename[64];
			for(int i=0; i <= 100; i++) {
				make_padded_name(tcasename, tc->name,
					MIN(size_t, tcase_pfx, 3) + 1, i);
				char combined[64];
				snprintf(combined, 64, "%s%s", suitename, tcasename);
				if(strset_get(&tcase_set, combined) == NULL) {
					strset_add(&tcase_set, talloc_strdup(tcctx, combined));
					break;
				}
			}

			size_t test_pfx = prefix_length(&tc->tests,
				offsetof(struct test, tcase_link),
				offsetof(struct test, name));
			struct test *t;
			list_for_each(&tc->tests, t, tcase_link) {
				char tid[64];
				for(int i=0; i <= 100; i++) {
					char testname[32];
					make_padded_name(testname, t->name,
						MIN(size_t, test_pfx, 3) + 1, i);
					snprintf(tid, sizeof(tid), "%s%s%s",
						suitename, tcasename, testname);
					if(strset_get(&id_set, tid) == NULL) {
						strset_add(&id_set, talloc_strdup(setctx, tid));
						break;
					}
				}

				int tid_len = strlen(tid);
				struct test_entry *ent = malloc(
					sizeof(struct test_entry) + tid_len + 1);
				memcpy(ent->path, tid, tid_len);
				ent->path[tid_len] = '\0';
				ent->s = s;
				ent->tc = tc;
				ent->test = t;
				size_t old_size = table->elems;
				assert(get_test_entry(table, ent->path) == NULL);
				bool ok = htable_add(table, hash_string(ent->path), ent);
				assert(ok);
				assert(table->elems == old_size + 1);
				assert(get_test_entry(table, ent->path) == ent);
			}
		}

		strset_clear(&tcase_set);
		talloc_free(tcctx);
	}

	strset_clear(&suite_set);
	strset_clear(&id_set);
	talloc_free(setctx);
}


/* lazy add. */
static void gen_test_ids(SRunner *sr) {
	if(sr->test_by_id.elems == 0) add_test_ids(&sr->test_by_id, sr);
}


void srunner_describe(SRunner *sr)
{
	gen_test_ids(sr);

	Suite *s;
	list_for_each(&sr->suites, s, runner_link) {
		printf("*** desc suite `%s'\n", s->name);

		TCase *tc;
		list_for_each(&s->cases, tc, suite_link) {
			printf("*** desc tcase `%s'\n", tc->name);

			struct test *t;
			list_for_each(&tc->tests, t, tcase_link) {
				/* TODO: this could be something besides a brute force search,
				 * if the test_ent structures were added to a hash table by
				 * int_hash(test ^ tcase ^ suite) pointers. but meh.
				 */
				bool found = false;
				struct htable_iter it;
				for(void *cur = htable_first(&sr->test_by_id, &it);
					cur != NULL && !found;
					cur = htable_next(&sr->test_by_id, &it))
				{
					struct test_entry *ent = cur;
					if(ent->test == t && ent->s == s && ent->tc == tc) {
						printf("*** desc test `%s' low:%d high:%d id:%s\n",
							t->name, t->low, t->high, ent->path);
						found = true;
					}
				}
				assert(found);
			}
		}
	}
}


static void begin_suite(Suite *s) {
	printf("*** begin suite `%s'\n", s->name);
}


static bool begin_tcase(TCase *tc)
{
	printf("*** begin tcase `%s'\n", tc->name);
	return list_empty(&tc->u_fixtures)
		|| run_fixture_list(&tc->u_fixtures, false);
}


static void run_test_in_case(
	TCase *tc, struct test *t, int iter,
	int report_mode)
{
	if(!tc->do_fork && !list_empty(&tc->c_fixtures)) {
		printf("*** tcase `%s' has checked fixtures, but do_fork=%s!\n",
			tc->name, btos(tc->do_fork));
		abort();
	}

	L4_ThreadId_t parent_tid = L4_Myself(), child_tid = L4_nilthread;
	int child = 0;
	if(tc->do_fork) {
		child = fork_tid(&child_tid);
		if(child == -1) {
			printf("*** fork failed\n");
			abort();
		}
	}

	bool failed = true;
	int rc = 0;
	if(child == 0) {
		printf("*** begin test `%s'", t->name);
		if(t->low < t->high) printf(" iter %d", iter); else assert(iter == 0);
		printf("\n");

		if(!list_empty(&tc->c_fixtures)
			&& !run_fixture_list(&tc->c_fixtures, false))
		{
			/* TODO: do an exit-to-restart here just in case the microkernel
			 * has become fucked. but note the test sequence run so far, or
			 * something.
			 *
			 * FIXME: at least signal that a test case was skipped!
			 *
			 * for now this just goes on to the next tcase, leaving fixtures
			 * before the failed one in place. that's pretty bad.
			 */
			printf("*** test `%s': checked fixture setup failed\n", t->name);
			goto end;
		}

		flush_log(false);
		tap_reset();
		failed = !run_test(tc, t, iter);

		rc = exit_status();
		if(rc > 0 || failed) {
			/* TODO: gather results for unplanned and unexecuted test points,
			 * report according to report_mode.
			 */
			flush_log(true);
		}

		if(!list_empty(&tc->c_fixtures)
			&& !run_fixture_list(&tc->c_fixtures, true))
		{
			printf("*** test `%s': checked fixture teardown failed\n", t->name);
			/* FIXME: do exit-to-restart or something */
		}

		if(tc->do_fork) {
			L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 2 }.raw);
			L4_LoadMR(1, (int)failed);
			L4_LoadMR(2, rc);
			L4_MsgTag_t tag = L4_Call(parent_tid);
			if(L4_IpcFailed(tag)) {
				printf("*** return status to parent failed: ec=%#lx\n",
					L4_ErrorCode());
				abort();
			}

			exit(rc);
		}
	} else {
		L4_MsgTag_t tag = L4_Receive(child_tid);
		if(L4_IpcFailed(tag)) {
			printf("*** receive from child failed: ec=%#lx\n",
				L4_ErrorCode());
			failed = true;
			rc = 0;		/* FIXME: set to something more telling */
		} else {
			L4_Word_t tmp;
			L4_StoreMR(1, &tmp); failed = (bool)tmp;
			L4_StoreMR(2, &tmp); rc = (int)tmp;
			L4_LoadMR(0, 0);
			L4_Reply(child_tid);
		}

		int status, dead_id;
		do {
			status = 0;
			dead_id = wait(&status);
			if(dead_id != child) {
				printf("*** unexpected dead child PID %d\n", dead_id);
			}
		} while(dead_id >= 0 && dead_id != child);
	}

end:
	if(failed) {
		printf("*** test `%s' failed, rc %d\n", t->name, rc);
	} else {
		printf("*** end test `%s' rc %d\n", t->name, rc);
	}
}


static bool end_tcase(TCase *tc)
{
	bool ok = list_empty(&tc->u_fixtures)
		|| run_fixture_list(&tc->u_fixtures, true);
	printf("*** end tcase `%s'\n", tc->name);
	return ok;
}


static void end_suite(Suite *s) {
	printf("*** end suite `%s'\n", s->name);
}


void srunner_run_path(SRunner *sr, const char *path, int report_mode)
{
	gen_test_ids(sr);

	int plen = strlen(path);
	char copy[plen + 1];
	memcpy(copy, path, plen + 1);

	/* separate iteration count from full test name. */
	int iter = 0;
	bool all_iters = false;
	char *colon = strrchr(copy, ':');
	if(colon != NULL && colon[1] == '*') {
		all_iters = true;
	} else if(colon != NULL && (colon[1] == '-' || isdigit(colon[1]))) {
		char *sep = colon + 1;
		bool neg = *sep == '-';
		if(neg) sep++;
		while(*sep != '\0') {
			if(!isdigit(*sep)) {
				printf("*** invalid integer `%s'\n", strrchr(path, ':') + 1);
				abort();
			}
			iter = iter * 10 + *(sep++) - '0';
		}
		if(neg) iter = -iter;
	}
	if(colon != NULL) *colon = '\0';

	assert(sr->test_by_id.elems == sr->test_by_path.elems);
	struct htable *tabs[] = { &sr->test_by_id, &sr->test_by_path };
	struct test_entry *ent = NULL;
	for(int i=0; i < NUM_ELEMENTS(tabs) && ent == NULL; i++) {
		ent = get_test_entry(tabs[i], copy);
#ifndef NDEBUG
		/* heavy debugging owing to past fuckery in strcmp(): check that @copy
		 * is indeed not in &sr->test_by_id .
		 */
		struct htable_iter it;
		for(struct test_entry *e = htable_first(&sr->test_by_id, &it);
			ent == NULL && e != NULL;
			e = htable_next(&sr->test_by_id, &it))
		{
			assert(!streq(copy, e->path));
		}
#endif
	}
	if(ent == NULL) {
		printf("*** test path-or-id `%s' not found\n", copy);
		abort();
	}

	begin_suite(ent->s);
	begin_tcase(ent->tc);
	if(!all_iters) {
		iter += ent->test->low;
		run_test_in_case(ent->tc, ent->test, iter, report_mode);
	} else {
		for(int i=ent->test->low; i <= ent->test->high; i++) {
			run_test_in_case(ent->tc, ent->test, i, report_mode);
		}
	}
	end_tcase(ent->tc);
	end_suite(ent->s);
}


/* TODO: run fixture setup and teardown in threads so that they can call
 * fail_unless() etc. also
 */
void srunner_run_all(SRunner *sr, int report_mode)
{
	Suite *s;
	list_for_each(&sr->suites, s, runner_link) {
		begin_suite(s);
		TCase *tc;
		list_for_each(&s->cases, tc, suite_link) {
			if(!begin_tcase(tc)) {
				/* FIXME: see below */
				printf("*** abort tcase `%s' (unchecked fixture failed)\n",
					tc->name);
				continue;
			}
			struct test *t;
			list_for_each(&tc->tests, t, tcase_link) {
				int high = t->high;
				if(high < t->low) high = t->low;
				for(int val = t->low; val <= high; val++) {
					run_test_in_case(tc, t, val, report_mode);
				}
			}
			if(!end_tcase(tc)) {
				printf("*** tcase `%s': unchecked fixture teardown failed\n",
					tc->name);
				/* FIXME: see above */
			}
		}
		end_suite(s);
	}
}
