/* tick_loop.c — GC-free audio block execution loop.
 *
 * Called from Haskell via FFI.  The tight loop dispatches each node's
 * kernel function pointer in topological order.  No Haskell callbacks
 * in the loop — pure C function pointer dispatch.
 */

#include "tick_loop.h"
#include <string.h>  /* memcpy for feedback swap */

void mkl_tick_once(MklExecutionPlan *plan)
{
    uint32_t i;
    MklNodeDesc *node;

    /* Execute every node in topological order. */
    for (i = 0; i < plan->n_nodes; i++) {
        node = &plan->nodes[i];
        if (node->kernel) {
            node->kernel(
                node->inputs,  node->n_inputs,
                node->outputs, node->n_outputs
            );
        }
    }

    /* Swap feedback buffer pointers (zero-copy). */
    for (i = 0; i < plan->n_feedback; i++) {
        MklBuffer *tmp         = plan->feedback_current[i];
        plan->feedback_current[i]  = plan->feedback_previous[i];
        plan->feedback_previous[i] = tmp;
    }
}

void mkl_tick_loop(MklExecutionPlan *plan, uint32_t block_count)
{
    uint32_t b;
    for (b = 0; b < block_count; b++) {
        mkl_tick_once(plan);
    }
}
