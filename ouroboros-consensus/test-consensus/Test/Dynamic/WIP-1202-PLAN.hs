{- |

Motivation:

The test infrastructure controls genesis keys and creates, manages, and
destroys nodes. The test infrastructure therefore emulates genesis key owners
and node operators. Currently, this emulation is simple: genesis keys are never
re-delegated and nodes never stop running. This document plans how to enrich
the test infrastructure to also emulate behaviors that involve delegation
certificates, which are only realistic if they are accompanied by node
restarts.

The goal, at least for now, is to emulate *careful* key owners and operators.

Assumed by the test infrastructure:

  * A delegation map is an injective function from genesis ("cold") keys to
    operational ("hot") keys, and no operational key is a genesis key.

  * The chain's ledger includes a delegation map.

  * The genesis chain's ledger delegation map is total, mapping every genesis
    key present in that genesis configuration.

  * A node's static configuration determines its operational key. A node
    operator can only "change" a node's operational key by restarting or
    replacing that node.

  * An honest node cannot forge a block if its operational key is not in the
    range of its current ledger's delegation map (subject to anachrony).

  * Each genesis key has a single owner, and their delegations form a linear
    history. These per-genesis key histories interleave to create the linear
    history of the whole *intended delegation map*. This map does not
    necessarily exist concretely, but it is an important concept: the latest
    intended delegation map is the delegation map the key owners and operators
    are collectively trying to establish in every chain in the net. They do so
    by generating sufficient delegation certificate transactions.

  * The genesis chain's ledger delegation map equals the initial intended
    delegation map.

  * Each ledger delegation map in the net's chains lags behind the intended
    delegation map until the chain selects blocks with up-to-date delegation
    certificates.

Guaranteed by the test infrastructure:

  * After restarting a node, the new instance will generate the delegation
    certificate transaction that will eventually allow it to lead as necessary
    until that certficate ends up in its immutable DB.

  * Invariant: No two node instances simultaneously have the same operational
    key.

  * Invariant: For all genesis keys @gk@, no two node instances simultaneously
    have a current ledger that maps @gk@ to their own operational key.

Mechanism:

We ensure that last invariant by always shutting down the relevant node
instance before altering the intended delegation map.

* Suppose we are changing the intended delegate map so that it maps @gk@ to
  @ok2@ instead of @ok1@.

* We would first shut down the node currently configured to use @ok1@, if any.

* We would then update the intended delegate map.

* We would then either restart the node or replace it with a new one; either
  way, the resulting node is (newly) configured to use @ok2@. (Restarting
  retains the file system, replacing does not.)

* We would generate the @gk := ok2@ delegate certificate transaction in the new
  node's memory pool. The new node cannot forge blocks until that transaction
  is in its own ledger, so it will only be able to propagate the transaction to
  its neighbors via TxSub. Only once it selects a chain (necessarily forged by
  a different node) that includes the transaction will it be able to forge
  again.

* We would continue generating that transaction until it's included in the new
  node's ledger. (A simpler and more artificial alternative would instead
  initially generate that transaction simultaneously in every node's mempool.)

Future Work:

  * One possible adversial behavior is for a new node to join the network with
    the same operational key as another.

Implementation Details:

The test infrastructure prior to Issue #1202 maintains two relatively flat
hierarchies of threads. The test infrastructure thread is the root of both, and
the hierarchies allow for the desirable use of combinators comparable to
@withAsync@.

  * In the first hierarchy, the test infrastructure spawns a thread for each
    undirected edge in the planned topology. Each such thread in turn spawns
    two threads, one for each directed edge. These in turn spawn mini protocol
    threads for the two peers of that directed edge. As a result, an exception
    from any of the mini protocol threads for either peer brings down all
    threads for the directed edge (some on both peers).

  * In the second hierarchy, the test infrastructure spawns each node's
    "internal" threads (e.g. StorageDB, block production, etc).

As a result of these separate hierarchies, termination of a node's internal
threads does not automatically terminate its mini protocol threads. (It
actually does, but that's only because any such termination is currently fatal
and brings down the whole test.)

As of Issue #1202, node instances will be stopped and restarted during the
test, which will also require that the threads for relevant edges be similarly
cycled. We must connect the two hierarchies somehow.

Before considering how to connect the hierarchies, we'll need a mechanism to
shutdown a node. We want to retain its data forever for inspection purposes,
but we do want to stop all of its threads. Currently, nodes allocate their
threads via a 'ResourceRegistry', and that registry is the only means of
closing the threads. However, the test infrastructure tracks all nodes' threads
in the same registry, so closing it to restart a single node is not an option.
With some additional care (e.g. paramterizing 'BlockchainTime' over the
'ResourceRegistry'), we can use a single registry per node. Closing that
registry will stop the node's internal threads and only those threads.

To connect the hierarchies, we want the restarting of a node instance to also
restart the relevant mini protocol instances. We use the following scheme.

  * The main test infrastructure thread spawns a thread for each vertex in the
    topology and a thread for each undirected edge in the topology. These
    threads do not terminate until the whole test terminates. Vertex threads
    emulate node operators, and undirected edge threads emulate the two
    vertices' cooperative connection management (abstracting away connects,
    disconnects, etc).

  * Each vertex thread allocates the resources that will persist across node
    restarts (e.g. the filesystem) and then spawns and @wait@s on a thread
    running a node instance.

  * Each node instance thread initializes and runs a node that uses the
    vertex's persistent resources (e.g. the filesystem). It also inserts the
    node's accessors into a shared variable so that the relevant undirected
    edge threads see that it is up and how to interact with it.

  * If and when the test configuration schedules for it do to so, a node
    instance thread will update the shared variable to indicate it is down,
    close its ChainDB, and raise a StopNodeInstance exception.

  * If the node instance thread raises a StopNodeInstance exception, then the
    vertex thread will replace it with a fresh node instance thread.

  * Each undirected edge thread blocks until its two node instances are
    simultaneously up, then @link@s to the two node instance threads, then
    spawns one thread for each of the two directed edges, and then @wait@s for
    either to terminate.

  * The undirected edge thread handles StopNodeInstance exceptions by looping.

  * Each directed edge thread spawns multiple threads, one for each mini
    protocol instance in the client-server pair, and then @wait@s for any to
    terminate (they currently only terminate by exception -- for example,
    ChainSync never sends the Done message).

  * The directed edge thread handles \"expected\" exceptions (e.g. ForkTooDeep)
    by blocking until the next slot onset and then looping.

  * Finally, the vertex thread and the undirect edge thread both re-interpret
    specific exceptions as StopNodeInstance exceptions if the relevant node
    instance is already down when they handle the exception. They similarly
    ignore ExceptionInLinkedThread wrappers. The re-interpretation is necessary
    because when the node instance thread raises a StopNodeInstance, its
    internal threads or clean-up actions may in turn raise a different
    exception, for example due to the node's ChainDB already being closed. If
    these were not reinterpreted, then the vertex thread would not restart the
    node instance. The mini protocol threads can similarly raise an exception
    during the node restart (e.g. once it closes its ChainDB) that may prevent
    the crucial StopNodeInstance exception from reaching the undirected edge
    thread.

  * Those threads might re-interpret an exception that only coincides with a
    node restart by chance. We consider that acceptable because the generator
    for test configuration plans should be able to trigger that same exact
    exception without a coincident node restart.

The following diagram illustrates the above scheme for the simplest topology:
two connected nodes.

  * This diagram is a snapshot of the threads in a respective steady-state: all
    are up, connected, and running.

  * It does not show the per-vertex shared variable that is updated by the
    vertex and read by its undirected edges.

  * It depicts the node instance threads and mini protocol threads as leaves
    even though they have resources and/or children of their own, because their
    internals are only relevant to the illustrated scheme as a source of
    exceptions.

  * Each point is a thread; upper-case threads only terminate once during the
    test.

  * Each path along @-@ and @|@ is a parent-child thread relationship, in which
    the child cannot outlive the parent and the child terminating via exception
    reraises it in the parent.

  * Each @>>>>>>@ is a @link@ relationship that forwards exceptions in the
    indicated direction. Note that this is weaker than a parent-child
    relationship.

  * Each @..@ is the channel over which two paired mini protocol threads
    exchange messages; it is a resource managed by the directed edge thread.

  * A @!@ indicates that the thread handles some exceptions by destroying and
    recreating all of its children and most if not all of its resources.

@
                  V!               V!
                  |                 |
                  ni>>>>>>UE!<<<<<<ni
                           |
             -------------------------------
            |                               |
           de!                             de!
            |                               |
  ----------------------         ----------------------
 |    |   |    |   |    |       |    |   |    |   |    |
cCS..sCS cBF..sBF cTX..sTX     cCS..sCS cBF..sBF cTX..sTX
@

Issue #1202 introduces the vertex threads (@V@), undirected edge threads
(@UE@), and the explicit links (@>>>@). These were previously unnecessary,
since the node instance threads (@ni@) and so the directed edge threads (@de@)
only terminated at the end of the test (thus they were denoted @NI@ and @DE@).

-}
