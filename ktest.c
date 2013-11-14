
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ccan/str/str.h>

#include <ukernel/ktest.h>

#if KTEST

static char *cur_suite = NULL, *cur_tcase = NULL;


static void end_tcase(void)
{
	printf("*** end tcase `%s'\n", cur_tcase);
	free(cur_tcase); cur_tcase = NULL;
}


static void end_suite(void)
{
	printf("*** end suite `%s'\n", cur_suite);
	free(cur_suite);
}


void _run_ktest(
	void (*testfn)(void),
	const char *file,
	const char *func,
	const char *testname)
{
	if(testfn == NULL) {
		/* flush current suite and tcase. */
		if(cur_tcase != NULL) end_tcase();
		if(cur_suite != NULL) end_suite();
		return;
	}

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
			printf("*** begin suite `%s'\n", cur_suite);
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
			printf("*** begin tcase `%s'\n", cur_tcase);
		}
	}

	if(strstarts(testname, "t_")) testname += 2;
	printf("*** begin test `%s'\n", testname);
	(*testfn)();
	printf("*** end test `%s' rc %d\n", testname, exit_status());
}


#define SUITE(name) \
	extern void ktest_##name(void); \
	ktest_##name();


void run_all_tests(void) {
	SUITE(kth);
	_run_ktest(NULL, NULL, NULL, NULL);
}

#endif
