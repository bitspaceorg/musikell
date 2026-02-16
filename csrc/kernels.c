/* kernels.c — C implementations of built-in audio kernels.
 *
 * These are the function pointers stored in MklNodeDesc for the
 * native tick loop path.  They operate on MklBuffer arrays directly.
 */

#include "tick_loop.h"
#include <math.h>
#include <string.h>

/* ─── Passthrough ────────────────────────────────────────────────── */

void mkl_kernel_passthru(
    MklBuffer *inputs,  uint32_t n_inputs,
    MklBuffer *outputs, uint32_t n_outputs)
{
    (void)n_inputs; (void)n_outputs;
    if (n_inputs < 1 || n_outputs < 1) return;
    uint32_t n = inputs[0].size < outputs[0].size
               ? inputs[0].size : outputs[0].size;
    memcpy(outputs[0].data, inputs[0].data, n * sizeof(float));
}

/* ─── Gain ───────────────────────────────────────────────────────── */

/* Gain state: single float multiplier, passed via inputs[n_inputs-1].data[0]
   (the last "input" is a 1-sample control buffer holding the gain value). */
void mkl_kernel_gain(
    MklBuffer *inputs,  uint32_t n_inputs,
    MklBuffer *outputs, uint32_t n_outputs)
{
    (void)n_outputs;
    if (n_inputs < 1 || n_outputs < 1) return;
    uint32_t n = inputs[0].size;
    float *in  = inputs[0].data;
    float *out = outputs[0].data;
    /* If there's a second input, use its first sample as gain. */
    float g = (n_inputs >= 2 && inputs[1].size >= 1)
            ? inputs[1].data[0] : 1.0f;
    for (uint32_t i = 0; i < n; i++) {
        out[i] = in[i] * g;
    }
}

/* ─── Mix (sum all inputs) ───────────────────────────────────────── */

void mkl_kernel_mix(
    MklBuffer *inputs,  uint32_t n_inputs,
    MklBuffer *outputs, uint32_t n_outputs)
{
    if (n_outputs < 1) return;
    uint32_t n = outputs[0].size;
    float *out = outputs[0].data;
    /* Zero output. */
    memset(out, 0, n * sizeof(float));
    /* Accumulate each input. */
    for (uint32_t j = 0; j < n_inputs; j++) {
        float *in = inputs[j].data;
        uint32_t len = inputs[j].size < n ? inputs[j].size : n;
        for (uint32_t i = 0; i < len; i++) {
            out[i] += in[i];
        }
    }
}

/* ─── Clip ───────────────────────────────────────────────────────── */

/* Clip bounds encoded in a 2-sample control buffer (inputs[1]). */
void mkl_kernel_clip(
    MklBuffer *inputs,  uint32_t n_inputs,
    MklBuffer *outputs, uint32_t n_outputs)
{
    (void)n_outputs;
    if (n_inputs < 1 || n_outputs < 1) return;
    uint32_t n = inputs[0].size;
    float *in  = inputs[0].data;
    float *out = outputs[0].data;
    float lo = -1.0f, hi = 1.0f;
    if (n_inputs >= 2 && inputs[1].size >= 2) {
        lo = inputs[1].data[0];
        hi = inputs[1].data[1];
    }
    for (uint32_t i = 0; i < n; i++) {
        float s = in[i];
        if (s < lo) s = lo;
        if (s > hi) s = hi;
        out[i] = s;
    }
}

/* ─── Sine oscillator ────────────────────────────────────────────── */

/* State: phase stored in outputs[0].data[-1] is not practical.
   Instead, the Haskell side pre-fills a state struct.  For now,
   this is a stateless version that takes freq via a control input. */
void mkl_kernel_sine(
    MklBuffer *inputs,  uint32_t n_inputs,
    MklBuffer *outputs, uint32_t n_outputs)
{
    (void)inputs; (void)n_inputs;
    if (n_outputs < 1) return;
    uint32_t n = outputs[0].size;
    float *out = outputs[0].data;
    /* Placeholder: generate 440 Hz at 44100 sr. */
    static double phase = 0.0;
    double freq = 440.0;
    double sr   = 44100.0;
    double inc  = freq / sr;
    for (uint32_t i = 0; i < n; i++) {
        out[i] = (float)sin(2.0 * M_PI * phase);
        phase += inc;
        if (phase >= 1.0) phase -= 1.0;
    }
}

/* ─── Ramp state (parameter smoothing) ───────────────────────────── */

typedef struct {
    float    current;
    float    target;
    float    step;
    uint32_t remaining;
} MklRampState;

static inline float mkl_ramp_advance(MklRampState *r)
{
    if (r->remaining == 0) return r->target;
    r->remaining--;
    if (r->remaining == 0) {
        r->current = r->target;
        r->step    = 0.0f;
        return r->target;
    }
    r->current += r->step;
    return r->current;
}

/* Gain kernel with per-sample ramp smoothing.
   The ramp state pointer is passed as user_data (via MklNodeDesc extension). */
void mkl_kernel_gain_smoothed(
    MklBuffer *inputs,  uint32_t n_inputs,
    MklBuffer *outputs, uint32_t n_outputs,
    MklRampState *ramp)
{
    (void)n_inputs; (void)n_outputs;
    if (n_inputs < 1 || n_outputs < 1 || !ramp) return;
    uint32_t n = inputs[0].size;
    float *in  = inputs[0].data;
    float *out = outputs[0].data;
    for (uint32_t i = 0; i < n; i++) {
        float g = mkl_ramp_advance(ramp);
        out[i] = in[i] * g;
    }
}

/* ─── Crossfade ──────────────────────────────────────────────────── */

void mkl_kernel_crossfade(
    MklBuffer *inputs,  uint32_t n_inputs,
    MklBuffer *outputs, uint32_t n_outputs)
{
    if (n_inputs < 2 || n_outputs < 1) return;
    uint32_t n = outputs[0].size;
    float *a   = inputs[0].data;
    float *b   = inputs[1].data;
    float *out = outputs[0].data;
    /* Linear crossfade over the block. */
    for (uint32_t i = 0; i < n; i++) {
        float t = (float)i / (float)(n > 1 ? n - 1 : 1);
        out[i] = a[i] * (1.0f - t) + b[i] * t;
    }
}
