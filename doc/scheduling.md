
This file documents scheduling in the ``mung'' microkernel.

[TODO: describe the following:
  - main scheduling states
  - the kernel-side scheduling mechanism
  - the Schedule system call
    - the per-thread ``scheduler'' property
  - quantum refilling
  - pre-emption
    - signaling
    - delaying]


The total_quantum variable
--------------------------

total_quantum carries a number of microseconds that a thread may run until it
is descheduled and its scheduler signaled pending a refresh. This lets
userspace implement multi-level feedback scheduling.

The microkernel's scheduler is sensitive to three modes of total_quantum. In
the first, tq > 0 ∧ tq < ∞. The scheduler will limit such a thread's maximum
single timeslice to its total_quantum, dock it as the thread leaves execution,
and deschedule the thread once it exhausts its total_quantum.

In the second tq=0, and the scheduler will not execute the thread under any
circumstance. In the third tq=∞, and the scheduler will neither limit the
thread's maximum single timeslice, reduce its tq, or refuse to run it because
of an exhausted total_quantum.

As this implies that a thread may execute only when its tq > 0, there must be
a state in which a thread is never selected by the scheduler, and which it may
return from when total_quantum is reset. ``mung'' encodes that state as
READY/descheduled, meaning that despite the thread's state being READY it is
not in the scheduling queue. Such threads always have tq=0. As they're in the
READY state, it's possible to halt and resume them, but this will only make
the thread move from READY to STOPPED and back; tq=0 keeps the READY thread
out of the queue.

To provide for the quantum-exhaustion event of feedback scheduling, L4.X2
specifies that when total_quantum reaches 0, the kernel causes the thread to
send an IPC message to its scheduler thread. The user-space scheduler may then
reassign it to a lower level in the queue as appropriate.


### Implementation ###

``mung'' handles the transition to tq=0 by setting up a kernel-generated
one-way IPC message from the affected thread to its scheduler, and switching
the thread to the READY/desched state in a post-exception hook call. This IPC
is subject to abort by a local ExchangeRegisters, and will appear as RUNNING
in the thread's scheduling status because the IPC originates in the kernel.

ThreadControl is tested to pass a modification test on threads that've entered
a tq=0 state, whether in IPC or after. This testing also covers halts and
restarts via ExchangeRegisters, and resumption by Schedule.


### Compatibility ###

The L4.X2 specification decrees the tq=0 IPC have SndTimeout=0, but since a
thread with tq=0 will never execute, ``mung'' sets SndTimeout=∞ instead. The
specification is silent on whether a reply is expected, so ``mung'' only
sends.

Similarly, the spec doesn't say what a freshly-created thread's total_quantum
is, so ``mung'' sets it to ∞. The default ts_len is 10 milliseconds.
