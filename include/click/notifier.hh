// -*- c-basic-offset: 4; related-file-name: "../../lib/notifier.cc" -*-
#ifndef CLICK_NOTIFIER_HH
#define CLICK_NOTIFIER_HH
#include <click/task.hh>
#include <click/atomic.hh>
#include <click/algorithm.hh>
#if HAVE_CXX_PRAGMA_INTERFACE
# pragma interface "click/notifier.hh"
#endif
CLICK_DECLS

class NotifierSignal {
  public:
    typedef bool (NotifierSignal::*unspecified_bool_type)() const;

     NotifierSignal();
     NotifierSignal(atomic_uint32_t* value, uint32_t mask);
     NotifierSignal(const NotifierSignal& x);
     ~NotifierSignal();

    static  NotifierSignal idle_signal();
    static  NotifierSignal busy_signal();
    static  NotifierSignal overderived_signal();
    static  NotifierSignal uninitialized_signal();

     bool active() const;
     operator unspecified_bool_type() const;

     bool set_active(bool active);

     bool idle() const;
     bool busy() const;
     bool overderived() const;
     bool initialized() const;

    friend bool operator==(const NotifierSignal &a, const NotifierSignal &b);
    friend bool operator!=(const NotifierSignal &a, const NotifierSignal &b);

    NotifierSignal& operator=(const NotifierSignal& x);
    NotifierSignal& operator+=(const NotifierSignal& x);
    friend NotifierSignal operator+(NotifierSignal a, const NotifierSignal &b);

     void swap(NotifierSignal& x);

    String unparse(Router *router) const;

    static void static_initialize();

  private:
    struct vmpair {
	atomic_uint32_t *value;
	uint32_t mask;
    };
    union vmvalue {
	atomic_uint32_t *v1;
	vmpair *vm;
    };

    vmvalue _v;
    uint32_t _mask;

    enum {
	true_mask = 1, false_mask = 2, overderived_mask = 4,
	uninitialized_mask = 8
    };
    static atomic_uint32_t static_value;

    void hard_assign_vm(const NotifierSignal &x);
    void hard_derive_one(atomic_uint32_t *value, uint32_t mask);
    static bool hard_equals(const vmpair *a, const vmpair *b);
};

class Notifier { public:

    enum SearchOp { SEARCH_STOP = 0, SEARCH_CONTINUE, SEARCH_CONTINUE_WAKE };
    typedef void (*callback_type)(void *, Notifier *);

     Notifier(SearchOp op = SEARCH_STOP);
     Notifier(const NotifierSignal &signal, SearchOp op = SEARCH_STOP);
    virtual ~Notifier();

    /** @brief Return whether the Notifier is initialized. */
     bool initialized() const {
	return _signal.initialized();
    }

    int initialize(const char *name, Router *router);

     const NotifierSignal &signal() const;
     SearchOp search_op() const;

     bool active() const;

     bool set_active(bool active);
     void wake();
     void sleep();

    virtual int add_activate_callback(callback_type f, void *user_data);
    virtual void remove_activate_callback(callback_type f, void *user_data);
     int add_listener(Task *task);
     void remove_listener(Task *task);
     int add_dependent_signal(NotifierSignal *signal);
     void remove_dependent_signal(NotifierSignal *signal);

    static const char EMPTY_NOTIFIER[];
    static const char FULL_NOTIFIER[];

    static  NotifierSignal upstream_empty_signal(Element *e, int port);
    static  NotifierSignal upstream_empty_signal(Element *e, int port, Task *task);
    static  NotifierSignal upstream_empty_signal(Element *e, int port, Notifier *dependent_notifier);
    static NotifierSignal upstream_empty_signal(Element *e, int port, callback_type f, void *user_data);

    static  NotifierSignal downstream_full_signal(Element *e, int port);
    static  NotifierSignal downstream_full_signal(Element *e, int port, Task *task);
    static  NotifierSignal downstream_full_signal(Element *e, int port, Notifier *dependent_notifier);
    static NotifierSignal downstream_full_signal(Element *e, int port, callback_type f, void *user_data);

    static  NotifierSignal upstream_empty_signal(Element *e, int port, int) CLICK_DEPRECATED;
    static  NotifierSignal upstream_empty_signal(Element *e, int port, int, Notifier *) CLICK_DEPRECATED;
    static  NotifierSignal downstream_full_signal(Element *e, int port, int) CLICK_DEPRECATED;
    static  NotifierSignal downstream_full_signal(Element *e, int port, int, Notifier *) CLICK_DEPRECATED;

  private:

    NotifierSignal _signal;
    SearchOp _search_op;

    static void dependent_signal_callback(void *, Notifier *);

};

class ActiveNotifier : public Notifier { public:

    ActiveNotifier(SearchOp op = SEARCH_STOP);
    ~ActiveNotifier();

    int add_activate_callback(callback_type f, void *v);
    void remove_activate_callback(callback_type f, void *v);
    void listeners(Vector<Task*> &v) const CLICK_DEPRECATED;

     void set_active(bool active, bool schedule = true);
     void wake();
     void sleep();

#if CLICK_DEBUG_SCHEDULING
    String unparse(Router *router) const;
#endif

  private:

    typedef union {
	Task *t;
	callback_type f;
	void *v;
	uintptr_t p;
    } task_or_signal_t;

    Task* _listener1;
    task_or_signal_t* _listeners;

    int listener_add(callback_type f, void *v);
    int listener_remove(callback_type f, void *v);

    ActiveNotifier(const ActiveNotifier&); // does not exist
    ActiveNotifier& operator=(const ActiveNotifier&); // does not exist

};

CLICK_ENDDECLS
#endif
