
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <ccan/str/str.h>

#include <ukernel/ktest.h>

#if KTEST

static char *cur_suite = NULL, *cur_tcase = NULL;
static bool describe_mode = false;


static void end_tcase(void)
{
	if(!describe_mode) {
		printf("*** end tcase `%s'\n", cur_tcase);
	}
	free(cur_tcase); cur_tcase = NULL;
}


static void end_suite(void)
{
	if(!describe_mode) {
		printf("*** end suite `%s'\n", cur_suite);
	}
	free(cur_suite); cur_suite = NULL;
}


void _run_ktest(
	const struct test_info *info,
	const char *file,
	const char *func)
{
	if(info == NULL) {
		/* flush current suite and tcase. */
		if(cur_tcase != NULL) end_tcase();
		if(cur_suite != NULL) end_suite();
		return;
	}

	const char *begin_str = describe_mode ? "desc" : "begin";
	int file_len = strlen(file);
	char suite[file_len + 1];
	strlcpy(suite, file, file_len);
	char *dot = strrchr(suite, '.');
	if(dot != NULL) *dot = '\0';
	if(cur_suite == NULL || !streq(cur_suite, suite)) {
		if(cur_tcase != NULL) end_tcase();
		if(cur_suite != NULL) end_suite();
		cur_suite = strdup(suite);
		if(cur_suite == NULL) {
			printf("can't set cur_suite: out of memory\n");
		} else {
			printf("*** %s suite `%s'\n", begin_str, cur_suite);
		}
	}

	const char *tcase = func;
	if(strstarts(tcase, "ktest_")) tcase += 6;
	if(cur_tcase == NULL || !streq(cur_tcase, tcase)) {
		if(cur_tcase != NULL) end_tcase();
		cur_tcase = strdup(tcase);
		if(cur_tcase == NULL) {
			printf("can't set cur_tcase: out of memory\n");
		} else {
			printf("*** %s tcase `%s'\n", begin_str, cur_tcase);
		}
	}

	const char *testname = info->name;
	if(strstarts(testname, "t_")) testname += 2;
	if(!describe_mode) {
		for(int i = info->iter_low; i <= info->iter_high; i++) {
			tap_reset();
			printf("*** begin test `%s'", testname);
			if(info->iter_low < info->iter_high) {
				printf(" iter %d", i);
			} else {
				assert(i == 0);
			}
			printf("\n");
			(*info->test_fn)(i);
			printf("*** end test `%s' rc %d\n", testname, exit_status());
		}
	} else {
		/* kernel tests cannot be selected by id, so this can be as long as
		 * it ends up being.
		 */
		char id[256];
		snprintf(id, sizeof(id),
			"%s.%s.%s", cur_suite, cur_tcase, testname);
		printf("*** desc test `%s' low:0 high:0 id:%s\n", testname, id);
	}
}


#define SUITE(name) \
	do { \
		extern void ktest_##name(void); \
		ktest_##name(); \
	} while(0)

void run_all_tests(void)
{
	SUITE(test_util);
	SUITE(vsnprintf);
	SUITE(slab);
	SUITE(heap);
	SUITE(memdesc);
}


void describe_all_tests(void)
{
	assert(!describe_mode);
	describe_mode = true;
	run_all_tests();
	describe_mode = false;
}

#endif
