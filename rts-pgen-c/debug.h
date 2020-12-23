#ifndef _DEBUG_H_
#define _DEBUG_H_

//Define an enumeration for log levels. Remember this needs to be ordered
typedef enum { LOG_NONE, LOG_ERROR, LOG_WARN, LOG_INFO, LOG_DEBUG } LogLevel;

//Helper macros to construct a LogLevel constant given the user-specified log level
//NOTE: The indirection helps when `a` is actually a macro.
#define USERLOGLEVELTOENUM(a) USERLOGLEVELTOENUMI(a)
#define USERLOGLEVELTOENUMI(a) ( LOG_ ## a )

//Make sure we have a default log level
#ifndef LOGLEVEL
    #define LOGLEVEL NONE
#endif

//Basic log macro that prints a message to the stderr. Note that this uses
//uses the value of CURRENTLOGLEVEL which can be set at compile time
//NOTE: We are have a non-standard use of `##__VA_ARGS__` here for convenience. But this
//is supported by GCC, Clang and ICC. If we ever decide to compile for MSVC, we need an
//alternate implementation.
//TODO: Should we include __FILE__, __LINE__, __func__ as is traditional in LOG messages
// (maybe with another compile time option)?
//TODO: Should we consider redirection to files here? Or is that an external concern
#define LOG(level, fmt, ...)                                             \
    do {                                                                 \
        if (USERLOGLEVELTOENUM(LOGLEVEL) >= LOG_##level) {               \
            fprintf(stderr, "[%s]", #level);                             \
            fprintf(stderr, " (%s:%s:%d)", __FILE__, __func__, __LINE__); \
            fprintf(stderr, " " fmt "\n", ##__VA_ARGS__);                \
        }                                                                \
    } while (0)

//Helpers for specific LOG levels
#define LOGE(...) LOG(ERROR, __VA_ARGS__)
#define LOGW(...) LOG(WARN, __VA_ARGS__)
#define LOGI(...) LOG(INFO, __VA_ARGS__)
#define LOGD(...) LOG(DEBUG, __VA_ARGS__)


//We enable ASSERT by default. You can control this explicitly by setting
//either `ENABLE_ASSERT=0` at compile time using some variation of `-D'
#if !defined(ENABLE_ASSERT)
    #define ASSERTSTATUS 1
#else
    #if ENABLE_ASSERT == 0
        #define ASSERTSTATUS 0
    #else
        #define ASSERTSTATUS 1
    #endif
#endif


#define ASSERTFAILURECODE 100

#define ASSERT(condition, fmt, ...)                                           \
    do {                                                                      \
        if (ASSERTSTATUS) {                                                   \
            if (!(condition)) {                                               \
                fprintf(stderr, " (%s:%s:%d)", __FILE__, __func__, __LINE__); \
                fprintf(stderr, " " fmt "\n", ##__VA_ARGS__);                 \
                exit(ASSERTFAILURECODE) ;                                     \
            }                                                                 \
        }                                                                     \
    } while (0)

#define ASSERT_FIELD(elem, field, fmt, ...)                                   \
    do {                                                                      \
        ASSERT(elem != NULL && elem->field != NULL, fmt, ##__VA_ARGS__);      \
    } while (0)


#endif /* _DEBUG_H_ */
