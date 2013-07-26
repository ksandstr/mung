
#define DROPPAGER_IMPL_SOURCE 1

#include <stdint.h>
#include <stdbool.h>
#include <ukernel/util.h>

#include <l4/types.h>
#include <l4/ipc.h>
#include <l4/space.h>

#include "defs.h"
#include "test.h"


#ifndef LOG_SIZE
#define LOG_SIZE DROP_PAGER_LOG_SIZE
#endif


struct drop_param
{
	int keep;
	int log_top;
	L4_Fpage_t log[LOG_SIZE];
};


static int ctx_key(void) {
	static int key = 0;
	if(key == 0) tsd_key_create(&key, &free);
	return key;
}


static struct drop_param *get_ctx(void) {
	struct drop_param *ptr = tsd_get(ctx_key());
	if(ptr == NULL) {
		ptr = calloc(1, sizeof(*ptr));
		for(int i=0; i < LOG_SIZE; i++) ptr->log[i] = L4_Nilpage;
		ptr->log_top = LOG_SIZE - 1;		/* start at 0 */
		tsd_set(ctx_key(), ptr);
	}
	return ptr;
}


static void set_params(int32_t n_keep)
{
	struct drop_param *p = get_ctx();
	p->keep = n_keep;
}


static void get_fault_log(L4_Fpage_t *faults_buf, unsigned *faults_len_p)
{
	struct drop_param *ctx = get_ctx();
	int o = 0;
	for(int i = 0, p = ctx->log_top;
		i < LOG_SIZE;
		i++, p = (p + 1) % LOG_SIZE)
	{
		assert(p > 0 && p < LOG_SIZE);
		assert(o < LOG_SIZE);
		if(!L4_IsNilFpage(ctx->log[p])) {
			faults_buf[o++] = ctx->log[p];
		}
	}
	*faults_len_p = o;
}


static void handle_fault(L4_Word_t faddr, L4_Word_t fip, L4_MapItem_t *map)
{
	struct drop_param *param = get_ctx();
	L4_MsgTag_t tag = muidl_get_tag();
	int rwx = tag.X.label & 0x000f;
#if 0
	L4_ThreadId_t from = muidl_get_sender();
	diag("drop_pager: pf in %lu:%lu at %#lx, ip %#lx",
		L4_ThreadNo(from), L4_Version(from), faddr, fip);
#endif
	param->log_top = (param->log_top + 1) % LOG_SIZE;
	param->log[param->log_top] = L4_FpageLog2(faddr, 12);
	L4_Set_Rights(&param->log[param->log_top], rwx);

	int dpos = param->log_top - param->keep;
	if(dpos < 0) dpos += LOG_SIZE;
	assert(dpos >= 0 && dpos < LOG_SIZE);
	L4_Fpage_t drop = param->log[dpos];
	if(!L4_IsNilFpage(drop)
		&& L4_Address(drop) != (faddr & ~PAGE_MASK))
	{
#if 0
		diag("flushing %#lx:%#lx (dpos %d)",
			L4_Address(drop), L4_Size(drop), dpos);
#endif
		L4_Set_Rights(&drop, L4_FullyAccessible);
		L4_FlushFpage(drop);
	}

	/* pass it on. */
	L4_LoadBR(0, L4_CompleteAddressSpace.raw);
	L4_LoadMR(0, (L4_MsgTag_t){ .X.label = 0xffe0 | rwx,
		.X.u = 2 }.raw);
	L4_LoadMR(1, faddr);
	L4_LoadMR(2, fip);
	tag = L4_Call(L4_Pager());
	if(L4_IpcFailed(tag)) {
		diag("drop-to-pager IPC failed, ec %lu",
			L4_ErrorCode());
		muidl_raise_no_reply();
	} else if(tag.X.t != 2 || tag.X.u != 0) {
		diag("drop-to-pager IPC returned weird tag %#lx", tag.raw);
		map->raw[0] = 0;
		map->raw[1] = 0;
	} else {
		/* AOK! */
		L4_StoreMRs(1, 2, map->raw);
	}
}


struct drop_pager_vtable pg_drop_vtab = {
	.quit = &idl_fixture_quit,
	.handle_fault = &handle_fault,
	.set_params = &set_params,
	.get_fault_log = &get_fault_log,
};
