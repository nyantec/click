// -*- c-basic-offset: 4; related-file-name: "../include/click/notifier.hh" -*-
/*
 * notifier.{cc,hh} -- activity notification
 * Eddie Kohler
 *
 * Copyright (c) 2002 International Computer Science Institute
 * Copyright (c) 2004-2005 Regents of the University of California
 * Copyright (c) 2008 Meraki, Inc.
 * Copyright (c) 2012 Eddie Kohler
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, subject to the conditions
 * listed in the Click LICENSE file. These conditions include: you must
 * preserve this copyright notice, and you cannot mention the copyright
 * holders in advertising related to the Software without their permission.
 * The Software is provided WITHOUT ANY WARRANTY, EXPRESS OR IMPLIED. This
 * notice is a summary of the Click LICENSE file; the license in that file is
 * legally binding.
 */

#include <click/config.h>
#if HAVE_CXX_PRAGMA_INTERFACE
# pragma implementation "click/notifier.hh"
#endif
#include <click/notifier.hh>
#include <click/router.hh>
#include <click/element.hh>
#include <click/routervisitor.hh>
#include <click/straccum.hh>
#include <click/bitvector.hh>
CLICK_DECLS

// should be const, but we need to explicitly initialize it
atomic_uint32_t NotifierSignal::static_value;
const char Notifier::EMPTY_NOTIFIER[] = "empty";
const char Notifier::FULL_NOTIFIER[] = "full";

/** @file notifier.hh
 * @brief Support for activity signals.
 */

/** @class NotifierSignal
 * @brief An activity signal.
 *
 * Activity signals in Click let one element determine whether another element
 * is active.  For example, consider an element @e X pulling from a @e Queue.
 * If the @e Queue is empty, there's no point in @e X trying to pull from it.
 * Thus, the @e Queue has an activity signal that's active when it contains
 * packets and inactive when it's empty.  @e X can check the activity signal
 * before pulling, and do something else if it's inactive.  Combined with the
 * sleep/wakeup functionality of ActiveNotifier, this can greatly reduce CPU
 * load due to polling.
 *
 * A "basic activity signal" is essentially a bit that's either on or off.
 * When it's on, the signal is active.  NotifierSignal can represent @e
 * derived activity signals as well.  A derived signal combines information
 * about @e N basic signals using the following invariant: If any of the basic
 * signals is active, then the derived signal is also active.  There are no
 * other guarantees; in particular, the derived signal might be active even if
 * @e none of the basic signals are active.
 *
 * Click elements construct NotifierSignal objects in four ways:
 *
 *  - idle_signal() returns a signal that's never active.
 *  - busy_signal() returns a signal that's always active.
 *  - Router::new_notifier_signal() creates a new basic signal.  This method
 *    should be preferred to NotifierSignal's own constructors.
 *  - operator+(NotifierSignal, const NotifierSignal &) creates a derived signal.
 */

/** @class Notifier
 * @brief A basic activity signal and notification provider.
 *
 * The Notifier class represents a basic activity signal associated with an
 * element.  Elements that contain a Notifier object will override
 * Element::cast() or Element::port_cast() to return that Notifier when given
 * the proper name.  This lets other parts of the configuration find the
 * Notifiers.  See upstream_empty_signal() and downstream_full_signal().
 *
 * The ActiveNotifier class, which derives from Notifier, can wake up clients
 * when its activity signal becomes active.
 */

/** @class ActiveNotifier
 * @brief A basic activity signal and notification provider that can
 * reschedule any dependent Task objects.
 *
 * ActiveNotifier, whose base class is Notifier, combines a basic activity
 * signal with the ability to wake up any dependent Task objects when that
 * signal becomes active.  Notifier clients are called @e listeners.  Each
 * listener corresponds to a Task object.  The listener generally goes to
 * sleep -- i.e., becomes unscheduled -- when it runs out of work and the
 * corresponding activity signal is inactive.  The ActiveNotifier class will
 * wake up the listener when it becomes active by rescheduling the relevant
 * Task.
 *
 * Elements that contain ActiveNotifier objects will generally override
 * Element::cast() or Element::port_cast(), allowing other parts of the
 * configuration to find the Notifiers.
 */


/** @brief Initialize the NotifierSignal implementation.
 *
 * This function must be called before NotifierSignal functionality is used.
 * It is safe to call it multiple times.
 *
 * @note Elements don't need to worry about static_initialize(); Click drivers
 * have already called it for you.
 */
void
NotifierSignal::static_initialize()
{
    static_value = true_mask | overderived_mask;
}

/** @brief Make this signal derived by adding information from @a x.
 * @param x the signal to add
 *
 * Creates a derived signal that combines information from this signal and
 * @a x.  Equivalent to "*this = (*this + @a x)".
 *
 * @sa operator+(NotifierSignal, const NotifierSignal&) */
NotifierSignal& NotifierSignal::operator+=(const NotifierSignal& x) {
    // preserve busy_signal(); adding other incompatible signals
    // leads to overderived_signal()
    if (idle() || (x.busy() && *this != busy_signal()) || !x.initialized())
	*this = x;
    else if (busy() || !initialized() || x.idle())
	/* do nothing */;
    else if (_mask && x._mask && _v.v1 == x._v.v1)
	_mask |= x._mask;
    else if (x._mask)
	hard_derive_one(x._v.v1, x._mask);
    else if (this != &x)
	for (vmpair *vm = x._v.vm; vm->mask; ++vm)
	    hard_derive_one(vm->value, vm->mask);

    return *this;
}

void
NotifierSignal::hard_assign_vm(const NotifierSignal &x)
{
    size_t n = 0;
    for (vmpair *vm = x._v.vm; vm->mask; ++vm)
	++n;
    if (likely((_v.vm = new vmpair[n + 1])))
	memcpy(_v.vm, x._v.vm, sizeof(vmpair) * (n + 1));
    else {
	// cannot call "*this = overderived_signal()" b/c _v.vm is invalid
	_v.v1 = &static_value;
	_mask = overderived_mask | true_mask;
    }
}

void
NotifierSignal::hard_derive_one(atomic_uint32_t *value, uint32_t mask)
{
    if (unlikely(_mask)) {
	if (busy())
	    return;
	if (_v.v1 == value) {
	    _mask |= mask;
	    return;
	}
	vmpair *vmp;
	if (unlikely(!(vmp = new vmpair[2]))) {
	    *this = overderived_signal();
	    return;
	}
	vmp[0].value = _v.v1;
	vmp[0].mask = _mask;
	vmp[1].mask = 0;
	_v.vm = vmp;
	_mask = 0;
    }

    size_t n, i;
    vmpair *vmp;
    for (i = 0, vmp = _v.vm; vmp->mask && vmp->value < value; ++i, ++vmp)
	/* do nothing */;
    if (vmp->mask && vmp->value == value) {
	vmp->mask |= mask;
	return;
    }
    for (n = i; vmp->mask; ++n, ++vmp)
	/* do nothing */;

    if (unlikely(!(vmp = new vmpair[n + 2]))) {
	*this = overderived_signal();
	return;
    }
    memcpy(vmp, _v.vm, sizeof(vmpair) * i);
    memcpy(vmp + i + 1, _v.vm + i, sizeof(vmpair) * (n + 1 - i));
    vmp[i].value = value;
    vmp[i].mask = mask;
    delete[] _v.vm;
    _v.vm = vmp;
}

bool
NotifierSignal::hard_equals(const vmpair *a, const vmpair *b)
{
    while (a->mask && a->mask == b->mask && a->value == b->value)
	++a, ++b;
    return !a->mask && a->mask == b->mask;
}

/** @brief Return a human-readable representation of the signal.
 * @param router the relevant router or null
 *
 * Useful for signal debugging. */
String NotifierSignal::unparse(Router* router) const {
    if (!_mask) {
	StringAccum sa;
	for (vmpair *vm = _v.vm; vm->mask; ++vm)
	    sa << (vm == _v.vm ? "" : "+")
	       << NotifierSignal(vm->value, vm->mask).unparse(router);
	return sa.take_string();
    }

    char buf[80];
    int pos;
    String s;
    if (_v.v1 == &static_value) {
	if (_mask == true_mask)
	    return "busy*";
	else if (_mask == false_mask)
	    return "idle";
	else if (_mask == overderived_mask)
	    return "overderived*";
	else if (_mask == uninitialized_mask)
	    return "uninitialized";
	else
	    pos = sprintf(buf, "internal/");
    } else if (router && (s = router->notifier_signal_name(_v.v1))) {
	pos = sprintf(buf, "%.52s/", s.c_str());
    } else
	pos = sprintf(buf, "@%p/", _v.v1);
    sprintf(buf + pos, active() ? "%x:%x*" : "%x:%x", _mask, (*_v.v1) & _mask);
    return String(buf);
}


/** @brief Destruct a Notifier. */
Notifier::~Notifier()
{
}

void
Notifier::dependent_signal_callback(void *user_data, Notifier *)
{
    NotifierSignal *signal = static_cast<NotifierSignal *>(user_data);
    signal->set_active(true);
}

/** @brief Register an activate callback with this Notifier.
 * @param f callback function
 * @param user_data callback data for @a f
 * @return 1 if notifier was added, 0 on other success, negative on error
 *
 * When this Notifier's associated signal is activated, this Notifier should
 * call @a f(@a user_data, this). Not all types of Notifier provide this
 * functionality. The default implementation does nothing.
 *
 * If @a f is null, then @a user_data is a Task pointer passed to
 * add_listener.
 *
 * @sa remove_activate_callback, add_listener, add_dependent_signal
 */
int
Notifier::add_activate_callback(callback_type f, void *user_data)
{
    (void) f, (void) user_data;
    return 0;
}

/** @brief Unregister an activate callback with this Notifier.
 * @param f callback function
 * @param user_data callback data for @a f
 *
 * Undoes the effect of all prior add_activate_callback(@a f, @a user_data)
 * calls. Does nothing if (@a f,@a user_data) was never added. The default
 * implementation does nothing.
 *
 * If @a f is null, then @a user_data is a Task pointer passed to
 * remove_listener.
 *
 * @sa add_activate_callback
 */
void
Notifier::remove_activate_callback(callback_type f, void *user_data)
{
    (void) f, (void) user_data;
}

/** @brief Initialize the associated NotifierSignal, if necessary.
 * @param name signal name
 * @param r associated router
 *
 * Initialize the Notifier's associated NotifierSignal by calling @a r's
 * Router::new_notifier_signal() method, obtaining a new basic activity
 * signal.  Does nothing if the signal is already initialized.
 */
int
Notifier::initialize(const char *name, Router *r)
{
    if (!_signal.initialized())
	return r->new_notifier_signal(name, _signal);
    else
	return 0;
}


/** @brief Construct an ActiveNotifier.
 * @param op controls notifier path search
 *
 * Constructs an ActiveNotifier object, analogous to the
 * Notifier::Notifier(SearchOp) constructor.  (See that constructor for more
 * information on @a op.)
 */
ActiveNotifier::ActiveNotifier(SearchOp op)
    : Notifier(op), _listener1(0), _listeners(0)
{
}

/** @brief Destroy an ActiveNotifier. */
ActiveNotifier::~ActiveNotifier()
{
    delete[] _listeners;
}

int
ActiveNotifier::add_activate_callback(callback_type f, void *v)
{
    // common case
    if (!_listener1 && !_listeners && !f) {
	_listener1 = static_cast<Task *>(v);
	return 1;
    }

    // count existing listeners
    int delta = 1;
    task_or_signal_t *tos = _listeners;
    for (; tos && tos->p; tos += delta)
	if (tos->p == 1) {
	    delta = 2;
	    --tos;
	} else if ((!f && delta == 1 && tos->t == static_cast<Task *>(v))
		   || (f && delta == 2 && tos->f == f && tos[1].v == v))
	    return 0;

    // create new listener array
    int n = tos - _listeners + 1;
    if (_listener1)
	++n;
    if (f)
	n += (delta == 2 ? 2 : 3);
    else
	++n;
    task_or_signal_t *ntos = new task_or_signal_t[n];
    if (!ntos) {
	click_chatter("out of memory in Notifier!");
	return -ENOMEM;
    }

    // populate listener array
    task_or_signal_t *otos = ntos;
    if (_listener1)
	(otos++)->t = _listener1;
    for (tos = _listeners; tos && tos->p > 1; ++tos)
	(otos++)->t = tos->t;
    if (!f)
	(otos++)->t = static_cast<Task *>(v);
    if (f || (tos && tos->p == 1)) {
	(otos++)->p = 1;
	if (tos && tos->p == 1)
	    for (++tos; tos->p > 0; ) {
		*otos++ = *tos++;
		*otos++ = *tos++;
	    }
	if (f) {
	    (otos++)->f = f;
	    (otos++)->v = v;
	}
    }
    (otos++)->p = 0;

    delete[] _listeners;
    _listeners = ntos;
    _listener1 = 0;
    return 1;
}

void
ActiveNotifier::remove_activate_callback(callback_type f, void *v)
{
    if (!f && _listener1 == static_cast<Task *>(v)) {
	_listener1 = 0;
	return;
    }
    int delta = 0, step = 1;
    task_or_signal_t *tos;
    for (tos = _listeners; tos && tos->p; )
	if ((!f && step == 1 && tos->t == static_cast<Task *>(v))
	    || (f && step == 2 && tos->f == f && tos[1].v == v)) {
	    delta = -step;
	    tos += step;
	} else {
	    if (delta)
		tos[delta] = *tos;
	    if (delta && step == 2)
		tos[delta + 1] = tos[1];
	    if (tos->p == 1) {
		++tos;
		step = 2;
	    } else
		tos += step;
	}
    if (delta != 0)
	tos[delta].p = 0;
}

/** @brief Return the listener list.
 * @param[out] v collects listener tasks
 *
 * Pushes all listener Task objects onto the end of @a v.
 */
void
ActiveNotifier::listeners(Vector<Task*>& v) const
{
    if (_listener1)
	v.push_back(_listener1);
    else if (_listeners)
	for (task_or_signal_t* l = _listeners; l->p > 1; ++l)
	    v.push_back(l->t);
}

#if CLICK_DEBUG_SCHEDULING
String
ActiveNotifier::unparse(Router *router) const
{
    StringAccum sa;
    sa << signal().unparse(router) << '\n';
    if (_listener1 || _listeners)
	for (int i = 0; _listener1 ? i == 0 : _listeners[i].p > 1; ++i) {
	    Task *t = _listener1 ? _listener1 : _listeners[i].t;
	    sa << "task " << ((void *) t) << ' ';
	    if (Element *e = t->element())
		sa << '[' << e->declaration() << "] ";
	    sa << (t->scheduled() ? "scheduled\n" : "unscheduled\n");
	}
    return sa.take_string();
}
#endif


namespace {

class NotifierRouterVisitor : public RouterVisitor { public:
    NotifierRouterVisitor(const char* name);
    bool visit(Element *e, bool isoutput, int port,
	       Element *from_e, int from_port, int distance);
    Vector<Notifier*> _notifiers;
    NotifierSignal _signal;
    bool _pass2;
    bool _need_pass2;
    const char* _name;
};

NotifierRouterVisitor::NotifierRouterVisitor(const char* name)
    : _signal(NotifierSignal::idle_signal()),
      _pass2(false), _need_pass2(false), _name(name)
{
}

bool
NotifierRouterVisitor::visit(Element* e, bool isoutput, int port,
			     Element *, int, int)
{
    if (Notifier* n = (Notifier*) (e->port_cast(isoutput, port, _name))) {
	if (find(_notifiers.begin(), _notifiers.end(), n) == _notifiers.end())
	    _notifiers.push_back(n);
	if (!n->signal().initialized())
	    n->initialize(_name, e->router());
	_signal += n->signal();
	Notifier::SearchOp search_op = n->search_op();
	if (search_op == Notifier::SEARCH_CONTINUE_WAKE && !_pass2) {
	    _need_pass2 = true;
	    return false;
	} else
	    return search_op != Notifier::SEARCH_STOP;

    } else if (port >= 0) {
	Bitvector flow;
	if (e->port_active(isoutput, port)) {
	    // went from pull <-> push
	    _signal = NotifierSignal::busy_signal();
	    return false;
	} else if ((e->port_flow(isoutput, port, &flow), flow.zero())
		   && e->flag_value('S') != 0) {
	    // ran out of ports, but element might generate packets
	    _signal = NotifierSignal::busy_signal();
	    return false;
	} else
	    return true;

    } else
	return true;
}

}

/** @brief Calculate and return the NotifierSignal derived from all empty
 * notifiers upstream of element @a e's input @a port.
 * @param e an element
 * @param port the input port of @a e at which to start the upstream search
 * @param f callback function
 * @param user_data user data for callback function
 * @sa add_activate_callback */
NotifierSignal
Notifier::upstream_empty_signal(Element* e, int port, callback_type f, void *user_data)
{
    NotifierRouterVisitor filter(EMPTY_NOTIFIER);
    int ok = e->router()->visit_upstream(e, port, &filter);

    NotifierSignal signal = filter._signal;

    // maybe run another pass
    if (ok >= 0 && signal != NotifierSignal() && filter._need_pass2) {
	filter._pass2 = true;
	ok = e->router()->visit_upstream(e, port, &filter);
    }

    // All bets are off if filter ran into a push output. That means there was
    // a regular Queue in the way (for example).
    if (ok < 0 || signal == NotifierSignal())
	return NotifierSignal();

    if (f || user_data)
	for (int i = 0; i < filter._notifiers.size(); i++)
	    filter._notifiers[i]->add_activate_callback(f, user_data);

    return signal;
}

/** @brief Calculate and return the NotifierSignal derived from all full
 * notifiers downstream of element @a e's output @a port.
 * @param e an element
 * @param port the output port of @a e at which to start the downstream search
 * @param f callback function
 * @param user_data user data for callback function
 * @sa add_activate_callback */
NotifierSignal
Notifier::downstream_full_signal(Element* e, int port, callback_type f, void *user_data)
{
    NotifierRouterVisitor filter(FULL_NOTIFIER);
    int ok = e->router()->visit_downstream(e, port, &filter);

    NotifierSignal signal = filter._signal;

    // maybe run another pass
    if (ok >= 0 && signal != NotifierSignal() && filter._need_pass2) {
	filter._pass2 = true;
	ok = e->router()->visit_downstream(e, port, &filter);
    }

    // All bets are off if filter ran into a pull input. That means there was
    // a regular Queue in the way (for example).
    if (ok < 0 || signal == NotifierSignal())
	return NotifierSignal();

    if (f || user_data)
	for (int i = 0; i < filter._notifiers.size(); i++)
	    filter._notifiers[i]->add_activate_callback(f, user_data);

    return signal;
}


/** @brief Construct a busy signal.
 *
 * The returned signal is always active. */
 NotifierSignal::NotifierSignal()
    : _mask(true_mask) {
    _v.v1 = &static_value;
}

/** @brief Construct an activity signal.
 *
 * Elements should not use this constructor directly.
 * @sa Router::new_notifier_signal */
 NotifierSignal::NotifierSignal(atomic_uint32_t* value, uint32_t mask)
    : _mask(mask) {
    _v.v1 = value;
}

/** @brief Copy construct a signal. */
 NotifierSignal::NotifierSignal(const NotifierSignal& x)
    : _mask(x._mask) {
    if (likely(_mask))
	_v.v1 = x._v.v1;
    else
	hard_assign_vm(x);
}

/** @brief Destroy a signal. */
 NotifierSignal::~NotifierSignal() {
    if (unlikely(_mask == 0))
	delete[] _v.vm;
}

/** @brief Return an idle signal.
 *
 * The returned signal is never active. */
 NotifierSignal NotifierSignal::idle_signal() {
    return NotifierSignal(&static_value, false_mask);
}

/** @brief Return a busy signal.
 *
 * The returned signal is always active. */
 NotifierSignal NotifierSignal::busy_signal() {
    return NotifierSignal(&static_value, true_mask);
}

/** @brief Return an overderived busy signal.
 *
 * Overderived signals replace derived signals that are too complex to
 * represent.  An overderived signal, like a busy signal, is always
 * active. */
 NotifierSignal NotifierSignal::overderived_signal() {
    return NotifierSignal(&static_value, overderived_mask | true_mask);
}

/** @brief Return an uninitialized signal.
 *
 * Uninitialized signals may be used occasionally as placeholders for true
 * signals to be added later.  Uninitialized signals are never active. */
 NotifierSignal NotifierSignal::uninitialized_signal() {
    return NotifierSignal(&static_value, uninitialized_mask);
}

/** @brief Test if the signal is active. */
 bool NotifierSignal::active() const {
    // 2012.May.16 This fence is necessary; consider, for example,
    // InfiniteSource's checking of nonfull notifiers.
    click_fence();
    if (likely(_mask))
	return (*_v.v1 & _mask) != 0;
    else {
	for (vmpair *vm = _v.vm; vm->mask; ++vm)
	    if ((*vm->value & vm->mask) != 0)
		return true;
	return false;
    }
}

/** @brief Test if the signal is active. */
 NotifierSignal::operator unspecified_bool_type() const {
    return active() ? &NotifierSignal::active : 0;
}

/** @brief Test if the signal is idle.
 * @return true iff the signal is idle, i.e. it will never be active. */
 bool NotifierSignal::idle() const {
    return (_mask == false_mask && _v.v1 == &static_value);
}

/** @brief Test if the signal is busy.
 * @return true iff the signal is busy, i.e. it will always be active.
 *
 * @note An overderived_signal() is busy(), but a busy_signal() is not
 * overderived(). */
 bool NotifierSignal::busy() const {
    return ((_mask & true_mask) && _v.v1 == &static_value);
}

/** @brief Test if the signal is overderived.
 * @return true iff the signal equals overderived_signal().
 *
 * @note An overderived_signal() is busy(), but a busy_signal() is not
 * overderived(). */
 bool NotifierSignal::overderived() const {
    return ((_mask & overderived_mask) && _v.v1 == &static_value);
}

/** @brief Test if the signal is initialized.
 * @return true iff the signal doesn't equal uninitialized_signal(). */
 bool NotifierSignal::initialized() const {
    return (!(_mask & uninitialized_mask) || _v.v1 != &static_value);
}

/** @brief Set whether the basic signal is active.
 * @param active true iff the basic signal is active
 * @return previous active state
 *
 * Use this function to set whether a basic signal is active.
 *
 * It is illegal to call set_active() on derived, idle, busy, or
 * overderived signals.  Some of these actions may cause an assertion
 * failure. */
 bool NotifierSignal::set_active(bool active) {
    assert(_v.v1 != &static_value && !(_mask & (_mask - 1)));
    uint32_t expected = *_v.v1;
#if !CLICK_USERLEVEL || HAVE_MULTITHREAD
    while (_mask) {
	uint32_t desired = (active ? expected | _mask : expected & ~_mask);
	uint32_t actual = _v.v1->compare_swap(expected, desired);
	if (expected == actual)
	    break;
	expected = actual;
    }
#else
    *_v.v1 = (active ? expected | _mask : expected & ~_mask);
#endif
    return expected & _mask;
}

/** @brief Assign a signal. */
 NotifierSignal& NotifierSignal::operator=(const NotifierSignal& x) {
    if (likely(this != &x)) {
	if (unlikely(_mask == 0))
	    delete[] _v.vm;
	_mask = x._mask;
	if (likely(_mask))
	    _v.v1 = x._v.v1;
	else
	    hard_assign_vm(x);
    }
    return *this;
}

/** @brief Exchange the values of this signal and @a x. */
 void NotifierSignal::swap(NotifierSignal& x) {
    click_swap(_v, x._v);
    click_swap(_mask, x._mask);
}

/** @relates NotifierSignal
 * @brief Test if two NotifierSignals are equal.
 *
 * Returns true iff the two NotifierSignals are the same -- i.e., they
 * combine information about exactly the same sets of basic signals.
 *
 * All idle() signals compare equal.  busy_signal() and
 * overderived_signal() do not compare equal, however. */
 bool operator==(const NotifierSignal& a, const NotifierSignal& b) {
    if (a._mask == b._mask) {
	if (likely(a._mask))
	    return a._v.v1 == b._v.v1;
	else
	    return NotifierSignal::hard_equals(a._v.vm, b._v.vm);
    } else
	return false;
}

/** @relates NotifierSignal
 * @brief Test if two NotifierSignals are unequal.
 *
 * Returns true iff !(@a a == @a b). */
 bool operator!=(const NotifierSignal& a, const NotifierSignal& b) {
    return !(a == b);
}

/** @relates NotifierSignal
 * @brief Return a derived signal.
 *
 * Returns a derived signal that combines information from its arguments.
 * The result will be active whenever @a a and/or @a b is active.  If the
 * combination of @a a and @a b is too complex to represent, returns an
 * overderived signal; this trivially follows the invariant since it is
 * always active.
 *
 * Signal derivation is commutative and associative.  The following
 * special combinations are worth remembering:
 *
 *  - An uninitialized signal plus any other signal is uninitialized.
 *  - An idle signal plus any signal @a a equals @a a.
 *  - A busy signal plus any other initialized signal is busy.
 *  - overderived_signal() plus busy_signal() equals busy_signal().
 *
 * @sa NotifierSignal::operator+= */
 NotifierSignal operator+(NotifierSignal a, const NotifierSignal& b) {
    return a += b;
}

/** @brief Constructs a Notifier.
 * @param op controls notifier path search
 *
 * This function constructs a Notifier object.  The Notifier's associated
 * NotifierSignal is initially idle; it becomes associated with a signal after
 * initialize() is called.
 *
 * The @a op argument controls path search.  The rest of this entry
 * describes it further.
 *
 * Elements interested in notification generally search for Notifier objects
 * along all possible packet paths upstream (or downstream) of one of their
 * ports.  When a Notifier is found along a path, further searching along that
 * path is cut off, so only the closest Notifiers are found.  Sometimes,
 * however, it makes more sense to continue searching for more Notifiers.  The
 * correct behavior is Notifier-specific, and is controlled by this method.
 * When the search encounters a Notifier, it consults the Notifier's @a
 * op variable supplied to the constructor.  It should equal one of
 * three SearchOp constants, which correspond to the following behavior:
 *
 * <dl>
 * <dt>SEARCH_STOP</dt>
 * <dd>Stop searching along this path.  This is the default.</dd>
 * <dt>SEARCH_CONTINUE</dt>
 * <dd>Continue searching along this path.</dd>
 * <dt>SEARCH_CONTINUE_WAKE</dt>
 * <dd>Continue searching along this path, but any further Notifiers should
 * only be used for adding and removing listeners; ignore their NotifierSignal
 * objects.  This operation is useful, for example, for schedulers that store
 * packets temporarily.  Such schedulers provide their own NotifierSignal,
 * since the scheduler may still hold a packet even when all upstream sources
 * are empty, but since they aren't packet sources, they don't know when
 * new packets arrive and can't wake up sleeping listeners.  During
 * initialization, such schedulers should call Notifier::upstream_empty_signal,
 * passing their own Notifier as the fourth argument.  This will ensure that
 * their signal is turned on appropriately whenever an upstream queue becomes
 * nonempty.</dd>
 * </dl>
 */
 Notifier::Notifier(SearchOp op)
    : _signal(NotifierSignal::uninitialized_signal()), _search_op(op) {
}

/** @brief Constructs a Notifier associated with a given signal.
 * @param signal the associated NotifierSignal
 * @param op controls notifier path search
 *
 * This function constructs a Notifier object associated with a specific
 * NotifierSignal, such as NotifierSignal::idle_signal().  Calling
 * initialize() on this Notifier will not change the associated
 * NotifierSignal.  The @a op argument is as in
 * Notifier::Notifier(SearchOp), above.
 */
 Notifier::Notifier(const NotifierSignal &signal, SearchOp op)
    : _signal(signal), _search_op(op) {
}

/** @brief Return this Notifier's associated NotifierSignal.
 *
 * Every Notifier object corresponds to one NotifierSignal; this method
 * returns it.  The signal is @link NotifierSignal::idle idle() @endlink
 * before initialize() is called.
 */
 const NotifierSignal& Notifier::signal() const {
    return _signal;
}

/** @brief Return this Notifier's search operation.
 *
 * @sa Notifier() for a detailed explanation of search operations.
 */
 Notifier::SearchOp Notifier::search_op() const {
    return _search_op;
}

/** @brief Returns whether the associated signal is active.
 *
 * Same as signal().active().
 */
 bool Notifier::active() const {
    return _signal.active();
}

/** @brief Set the associated signal's activity.
 * @param active true iff the signal should be active
 * @return previous active state
 */
 bool Notifier::set_active(bool active) {
    return _signal.set_active(active);
}

/** @brief Set the associated signal to active.
 * @sa set_active
 */
 void Notifier::wake() {
    set_active(true);
}

/** @brief Set the associated signal to inactive.
 * @sa set_active
 */
 void Notifier::sleep() {
    set_active(false);
}

/** @brief Register a listener with this Notifier.
 * @param task Task to reschedule when this Notifier becomes active
 *
 * When this Notifier's associated signal is activated, the Notifier should
 * schedule @a task. Not all types of Notifier provide this functionality. The
 * default implementation does nothing.
 *
 * @sa remove_listener, add_activate_callback, add_dependent_signal
 */
 int Notifier::add_listener(Task* task) {
    return add_activate_callback(0, task);
}

/** @brief Unregister a listener with this Notifier.
 * @param task listener Task
 *
 * Undoes the effect of all prior add_listener(@a task) calls. Does nothing if
 * @a task was never added. The default implementation does nothing.
 *
 * @sa add_listener
 */
 void Notifier::remove_listener(Task* task) {
    remove_activate_callback(0, task);
}

/** @brief Register a dependent signal with this Notifier.
 * @param signal dependent signal
 *
 * When this Notifier's associated signal is activated, the Notifier should
 * also activate @a signal. Not all types of Notifier provide this
 * functionality. The default implementation does nothing.
 *
 * @sa add_listener, add_activate_callback, remove_dependent_signal
 */
 int Notifier::add_dependent_signal(NotifierSignal* signal) {
    return add_activate_callback(dependent_signal_callback, signal);
}

/** @brief Unregister a dependent signal with this Notifier.
 * @param signal dependent signal
 *
 * Undoes the effect of all prior add_dependent_signal(@a signal) calls. Does
 * nothing if @a signal was never added. The default implementation does
 * nothing.
 *
 * @sa add_dependent_signal
 */
 void Notifier::remove_dependent_signal(NotifierSignal* signal) {
    remove_activate_callback(dependent_signal_callback, signal);
}

/** @brief Calculate and return the NotifierSignal derived from all empty
 * notifiers upstream of element @a e's input @a port.
 * @param e an element
 * @param port the input port of @a e at which to start the upstream search
 *
 * Searches the configuration upstream of element @a e's input @a port for @e
 * empty @e notifiers.  These notifiers are associated with packet storage,
 * and should be true when packets are available (or likely to be available
 * quite soon), and false when they are not.  All notifiers found are combined
 * into a single derived signal.  Thus, if any of the base notifiers are
 * active, indicating that at least one packet is available upstream, the
 * derived signal will also be active.  Element @a e's code generally uses the
 * resulting signal to decide whether or not to reschedule itself.
 *
 * The returned signal is generally conservative, meaning that the signal
 * is true whenever a packet exists upstream, but the elements that provide
 * notification are responsible for ensuring this.
 *
 * Overloaded versions of this function can also register a task (as in
 * add_listener()), a signal (as in add_dependent_notifier()), or a callback
 * function (as in add_active_callback()) for each located notifier. When
 * packets become available, the task will be scheduled, the signal will be
 * activated, or the callback will be called.
 *
 * <h3>Supporting upstream_empty_signal()</h3>
 *
 * Elements that have an empty notifier must override the Element::cast()
 * method.  When passed the @a name Notifier::EMPTY_NOTIFIER, this method
 * should return a pointer to the corresponding Notifier object.
 *
 * @sa downstream_full_signal
 */
 NotifierSignal Notifier::upstream_empty_signal(Element* e, int port) {
    return upstream_empty_signal(e, port, (callback_type) 0, 0);
}

/** @brief Calculate and return the NotifierSignal derived from all empty
 * notifiers upstream of element @a e's input @a port.
 * @param e an element
 * @param port the input port of @a e at which to start the upstream search
 * @param task task to schedule when packets become available
 * @sa add_listener */
 NotifierSignal Notifier::upstream_empty_signal(Element* e, int port,
                                                      Task* task) {
    return upstream_empty_signal(e, port, (callback_type) 0, task);
}

/** @brief Calculate and return the NotifierSignal derived from all empty
 * notifiers upstream of element @a e's input @a port.
 * @param e an element
 * @param port the input port of @a e at which to start the upstream search
 * @param notifier notifier to activate when packets become available
 * @sa add_dependent_signal */
 NotifierSignal Notifier::upstream_empty_signal(Element* e, int port,
                                                      Notifier* dependent_notifier) {
    return upstream_empty_signal(e, port, dependent_signal_callback, &dependent_notifier->_signal);
}

/** @brief Calculate and return the NotifierSignal derived from all full
 * notifiers downstream of element @a e's output @a port.
 * @param e an element
 * @param port the output port of @a e at which to start the downstream search
 *
 * Searches the configuration downstream of element @a e's output @a port for
 * @e full @e notifiers.  These notifiers are associated with packet storage,
 * and should be true when there is space for at least one packet, and false
 * when there is not.  All notifiers found are combined into a single derived
 * signal.  Thus, if any of the base notifiers are active, indicating that at
 * least one path has available space, the derived signal will also be active.
 * Element @a e's code generally uses the resulting signal to decide whether
 * or not to reschedule itself.
 *
 * Overloaded versions of this function can also register a task (as in
 * add_listener()), a signal (as in add_dependent_notifier()), or a callback
 * function (as in add_active_callback()) for each located notifier. When
 * space becomes available, the task will be scheduled, the signal will be
 * activated, or the callback will be called.
 *
 * In current Click, the returned signal is conservative: if it's inactive,
 * then there is no space for packets downstream.
 *
 * <h3>Supporting downstream_full_signal()</h3>
 *
 * Elements that have a full notifier must override the Element::cast()
 * method.  When passed the @a name Notifier::FULL_NOTIFIER, this method
 * should return a pointer to the corresponding Notifier object.
 *
 * @sa upstream_empty_signal
 */
 NotifierSignal Notifier::downstream_full_signal(Element* e, int port) {
    return downstream_full_signal(e, port, (callback_type) 0, 0);
}

/** @brief Calculate and return the NotifierSignal derived from all full
 * notifiers downstream of element @a e's output @a port.
 * @param e an element
 * @param port the output port of @a e at which to start the downstream search
 * @param task task to schedule when packets become available
 * @sa add_listener */
 NotifierSignal Notifier::downstream_full_signal(Element* e, int port,
                                                       Task* task) {
    return downstream_full_signal(e, port, (callback_type) 0, task);
}

/** @brief Calculate and return the NotifierSignal derived from all full
 * notifiers downstream of element @a e's output @a port.
 * @param e an element
 * @param port the output port of @a e at which to start the downstream search
 * @param notifier notifier to activate when packets become available
 * @sa add_dependent_signal */
 NotifierSignal Notifier::downstream_full_signal(Element* e, int port,
                                                       Notifier* dependent_notifier) {
    return downstream_full_signal(e, port, dependent_signal_callback, &dependent_notifier->_signal);
}

/** @cond never */
 NotifierSignal Notifier::upstream_empty_signal(Element* e, int port, int x) {
    (void) x;
    assert(x == 0);
    return upstream_empty_signal(e, port);
}

 NotifierSignal Notifier::upstream_empty_signal(Element* e, int port, int x,
                                                      Notifier* notifier) {
    (void) x;
    assert(x == 0);
    return upstream_empty_signal(e, port, notifier);
}

 NotifierSignal Notifier::downstream_full_signal(Element* e, int port, int x) {
    (void) x;
    assert(x == 0);
    return downstream_full_signal(e, port);
}

 NotifierSignal Notifier::downstream_full_signal(Element* e, int port, int x,
                                                       Notifier* notifier) {
    (void) x;
    assert(x == 0);
    return downstream_full_signal(e, port, notifier);
}
/** @endcond never */

/** @brief Set the associated signal's activity, possibly scheduling any
 * listener tasks.
 * @param active true iff the signal should be active
 * @param schedule if true, wake up listener tasks
 *
 * If @a active and @a schedule are both true, and the signal was previously
 * inactive, then any listener Tasks are scheduled with Task::reschedule().
 *
 * @sa wake, sleep, add_listener
 */
 void ActiveNotifier::set_active(bool active, bool schedule) {
    bool was_active = Notifier::set_active(active);
    if (active && schedule && !was_active) {
	// 2007.Sep.6: Perhaps there was a race condition here.  Make sure
	// that we set the notifier to active BEFORE rescheduling downstream
	// tasks.  This is because, in a multithreaded environment, a task we
	// reschedule might run BEFORE we set the notifier; after which it
	// would go to sleep forever.
	if (_listener1)
	    _listener1->reschedule();
	else if (task_or_signal_t *tos = _listeners) {
	    for (; tos->p > 1; tos++)
		tos->t->reschedule();
	    if (tos->p == 1)
		for (tos++; tos->p; tos += 2)
		    tos->f(tos[1].v, this);
	}
    }
}

/** @brief Set the associated signal to active and schedule any listener
 * tasks.
 *
 * If the signal was previously inactive, then any listener Tasks are
 * scheduled with Task::reschedule().
 *
 * @sa set_active, add_listener
 */
 void ActiveNotifier::wake() {
    set_active(true, true);
}

/** @brief Set the associated signal to inactive.
 * @sa set_active
 */
 void ActiveNotifier::sleep() {
    set_active(false, true);
}

 void click_swap(NotifierSignal& x, NotifierSignal& y) {
    x.swap(y);
}

CLICK_ENDDECLS
