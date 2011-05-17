This Ruby extension is the third iteration of SSH transport in virt-p2v. For
context, the first two were:

1. Net::SSH

Net::SSH is a pure Ruby implementation of SSH. It provides a clean interface and
is quite easy to use. Unfortunately, being pure Ruby it is both fantastically
slow and an independent implementation of a cryptographic library. The former
makes it unsuitable for bulk data transfer, the latter is a major security flaw.

2. Forked SSH

This also required a custom extension for handling PTYs, as ruby's PTY module
just isn't useful for long running processes. However, the major problem with it
was that it still required feeding bulk data to the process from Ruby. If you
spawn a separate ruby thread for it's either unusably slow if you run it at
regular priority, or runs to the total exclusion of the GUI thread if you run it
at elevated priority, and still isn't fast.

Ruby 1.8 doesn't have real threads: it has co-operative threads which run in a
single real thread, scheduled by the interpreter's own scheduler. This is
unsuitable for this task. Bulk SSH transfer is both IO and CPU intensive, and we
need to ensure that the GUI thread remains responsive.

This extension provides wrappers round a minimal set of libssh2 API calls. The
interface provided is synchronous, but all work is done is a real, posix thread.
The main thread executes the Ruby scheduler while it waits for the worker thread
to complete. This means the work is truly multithreaded, and other ruby threads
can continue to run. The data transfer performance of this module is equivalent
to the performance of scp on my hardware, which I have taken to mean it runs as
fast as is practical.
