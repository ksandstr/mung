
Microkernel features implemented in mung.
-----------------------------------------

This document documents them.


holeunmap
=========

Indicates that holes covered by Map or Grant items in Ipc will create holes in
the receiving space. This makes explicit the reading of the spec which
discusses only pages that exist, and unmaps ahead of them.

The rationale for this reading is that with its opposite, i.e. where holes in
the sender's range leave existing maps in the receiver, a redirector would
need to check for typed transfers and if those are present, clear out its
accept range with Unmap. This'd do away with an opportunity to use the
ReplyWait form of Ipc without branching, or when typed transfers are present;
both are undesirable for a redirector which may pass tens of percent of all
IPC traffic in the system.


zomgwallhax
===========

Indicates that the feature string mechanism works. The feature string
mechanism may still work even when "zomgwallhax" isn't present.
