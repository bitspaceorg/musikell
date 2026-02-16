/* tick_loop.h — C tick loop for GC-free audio block execution.
 *
 * The Haskell side serialises the execution plan into a flat array of
 * MklNodeDesc structs, each containing a kernel function pointer and
 * pre-resolved buffer pointers.  mkl_tick_once() walks the array and
 * calls each kernel in topological order.
 */

#ifndef MKL_TICK_LOOP_H
#define MKL_TICK_LOOP_H

#include <stdint.h>

/* ─── Buffer descriptor ──────────────────────────────────────────── */

typedef struct {
    float   *data;
    uint32_t size;       /* number of samples */
    uint32_t channels;
} MklBuffer;

/* ─── Kernel function pointer ────────────────────────────────────── */

/* Same signature as the ABI-contract execute function, but operates
   on MklBuffer* arrays rather than CBuffer* (layout-compatible). */
typedef void (*MklKernelFn)(
    MklBuffer *inputs,  uint32_t n_inputs,
    MklBuffer *outputs, uint32_t n_outputs
);

/* ─── Node descriptor ────────────────────────────────────────────── */

typedef struct {
    MklKernelFn  kernel;
    MklBuffer   *inputs;
    uint32_t     n_inputs;
    MklBuffer   *outputs;
    uint32_t     n_outputs;
} MklNodeDesc;

/* ─── Execution plan ─────────────────────────────────────────────── */

typedef struct {
    MklNodeDesc *nodes;
    uint32_t     n_nodes;
    /* Feedback buffer pairs: after each tick, swap current ↔ previous. */
    MklBuffer  **feedback_current;
    MklBuffer  **feedback_previous;
    uint32_t     n_feedback;
} MklExecutionPlan;

/* ─── API ────────────────────────────────────────────────────────── */

/* Execute one block through the entire graph.  Pure C, no GHC calls. */
void mkl_tick_once(MklExecutionPlan *plan);

/* Execute N blocks in a tight loop. */
void mkl_tick_loop(MklExecutionPlan *plan, uint32_t block_count);

#endif /* MKL_TICK_LOOP_H */
