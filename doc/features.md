
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


closed\_propagation
===================

The microkernel will permit propagation where otherwise forbidden if the
virtual sender is in a closed wait to the actual sender. This occurs
regardless of whether virtual sender and propagator, or virtual sender and
recipient are in the same address space, or if the propagator is in the
virtual sender's redirection chain.


zomgwallhax
===========

Indicates that the feature string mechanism works. The feature string
mechanism may still work even when "zomgwallhax" isn't present.
