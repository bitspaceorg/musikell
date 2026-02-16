/*
 * Musikell C ABI Contract
 * Version: 1
 *
 * This header defines the stable C ABI for Musikell backends.
 * External backend authors can implement this contract to create
 * custom backends without modifying the Musikell core.
 *
 * STABILITY GUARANTEE:
 * - The ABI version will be incremented for breaking changes
 * - Additive changes (new fields at end of structs) are non-breaking
 * - Field order and sizes are guaranteed within a major version
 *
 * MEMORY OWNERSHIP:
 * - Input buffers are owned by the caller (Musikell runtime)
 * - Output buffers are owned by the caller (pre-allocated)
 * - Kernels must not allocate or free buffer memory
 * - Kernels must not store references to buffers
 *
 * THREADING:
 * - Kernels may be called from any thread
 * - Kernels must be thread-safe if stateful
 * - The runtime guarantees no concurrent calls to the same kernel instance
 */

#ifndef MUSIKELL_ABI_H
#define MUSIKELL_ABI_H

#include <stdint.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ABI version for compatibility checking */
#define MUSIKELL_ABI_VERSION 1

/* Return codes */
#define MKL_OK              0
#define MKL_ERR_INVALID    -1
#define MKL_ERR_OVERFLOW   -2
#define MKL_ERR_INTERNAL   -3

/*
 * Buffer structure
 *
 * Represents a contiguous block of audio samples.
 * All samples are 32-bit IEEE 754 floating point.
 * Sample values are normalized to [-1.0, 1.0].
 */
typedef struct {
    float*   data;       /* Pointer to sample data (not owned) */
    uint32_t size;       /* Number of samples */
    uint32_t alignment;  /* Memory alignment in bytes */
    uint32_t channels;   /* Number of interleaved channels */
} MklBuffer;

/*
 * Kernel function signature
 *
 * A kernel processes input buffers and produces output buffers.
 * - inputs: Array of input buffer pointers (read-only)
 * - input_count: Number of input buffers
 * - outputs: Array of output buffer pointers (pre-allocated, write)
 * - output_count: Number of output buffers
 *
 * Returns: MKL_OK on success, error code otherwise
 */
typedef int32_t (*MklKernelFn)(
    const MklBuffer* inputs,
    uint32_t         input_count,
    MklBuffer*       outputs,
    uint32_t         output_count
);

/*
 * Kernel descriptor
 *
 * Describes a kernel's metadata and entry point.
 */
typedef struct {
    uint32_t     abi_version;    /* Must equal MUSIKELL_ABI_VERSION */
    const char*  kernel_name;    /* Unique kernel identifier (null-terminated) */
    const char*  kernel_version; /* Semantic version string */
    uint32_t     input_count;    /* Expected number of inputs */
    uint32_t     output_count;   /* Expected number of outputs */
    MklKernelFn  execute;        /* Kernel execution function */
    void*        user_data;      /* Optional user data (passed to execute) */
} MklKernelDescriptor;

/*
 * Backend descriptor
 *
 * Describes a backend's capabilities and entry points.
 */
typedef struct {
    uint32_t    abi_version;     /* Must equal MUSIKELL_ABI_VERSION */
    const char* backend_name;    /* Unique backend identifier */
    const char* backend_version; /* Semantic version string */

    /* Initialization */
    int32_t (*init)(void** context);
    int32_t (*shutdown)(void* context);

    /* Kernel management */
    int32_t (*register_kernel)(void* context, const MklKernelDescriptor* desc);
    int32_t (*execute_kernel)(void* context, const char* kernel_name,
                              const MklBuffer* inputs, uint32_t input_count,
                              MklBuffer* outputs, uint32_t output_count);
} MklBackendDescriptor;

/*
 * Runtime information query
 *
 * Backends can query runtime parameters.
 */
typedef struct {
    uint32_t block_size;    /* Samples per block */
    uint32_t sample_rate;   /* Samples per second */
    uint32_t channel_count; /* Number of channels */
} MklRuntimeInfo;

/*
 * Entry point for dynamic backend loading
 *
 * Backend shared libraries must export this function.
 * The runtime calls it to obtain the backend descriptor.
 */
typedef const MklBackendDescriptor* (*MklGetBackendFn)(void);

/*
 * Kernel registration entry point
 *
 * Backend shared libraries must export a function with this signature
 * named "mkl_register_kernels". The runtime calls it after dlopen to
 * discover all kernels the backend provides.
 *
 * The backend returns a pointer to a static array of kernel descriptors
 * and writes the number of descriptors into *count.
 *
 * The returned array must remain valid for the lifetime of the loaded library.
 */
typedef MklKernelDescriptor* (*MklRegisterKernelsFn)(uint32_t* count);

#ifdef __cplusplus
}
#endif

#endif /* MUSIKELL_ABI_H */
