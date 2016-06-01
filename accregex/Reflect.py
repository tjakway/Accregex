#get all methods for which comp returns true
#comp takes in a string and returns True or False
def _get_comp_methods(obj, comp):
    methods = []
    for x in dir(obj):
        if comp(x):
            methods.append(getattr(obj, x))
    return methods

def _get_comp_strings(obj, comp):
    strings = []
    for x in dir(obj):
        if comp(x):
            strings.append(x)
    return strings

def g_comp(x):
    x.startswith("Get")

def s_comp(x):
    x.startswith("Set")

def getter_strings(obj):
    return _get_comp_strings(obj, g_comp)

def setter_strings(obj):
    return _get_comp_strings(obj, s_comp)

def getter_methods(obj):
    return _get_comp_methods(obj, g_comp)

def setter_methods(obj):
    return _get_comp_methods(obj, s_comp)

#this is a huge hack for the lack of a copy constructor in Split
#but without it I'd have to manually type out a.SetFoo(b.GetFoo()) for every method under the sun
#and I make too many typos for that to be a good idea
#THE FIRST ARGUMENT IS MUTATED!
def chain_mutations(a, b):
    #! DO NOT DO THIS IF THE TYPES DO NOT MATCH!
    assert type(a) == type(b)
    getter_strs = getter_strings(b)
    setter_strs = setter_strings(a)

    gs, ss = filter_for_only_reciprocals(getter_strs, setter_strs)

    all_getters = getter_methods(gs)
    all_setters = setter_methods(ss)

    for t in zip(all_setters, all_getters):
        f, g = t
        a.f(b.g())

def filter_for_only_reciprocals(gs, ss):
    new_getters = set()
    new_setters = set()
    for g in gs:
        this_method_suffix = g.split("Get")[1]
        for s in ss:
            if s.split("Set")[1]:
                new_getters.append(g)
                new_setters.append(s)

    return (sorted(list(new_getters)), sorted(list(new_setters)))
