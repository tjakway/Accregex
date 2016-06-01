#get all methods for which comp returns true
#comp takes in a string and returns True or False
def _get_comp_methods(obj, comp):
    methods = []
    for x in dir(obj):
        if comp(x):
            methods.append(getattr(obj, x))
    return methods

def getters(obj):
    return _get_comp_methods(obj, lambda x: x.startswith("Get"))

def setters(obj):
    return _get_comp_methods(obj, lambda x: x.startswith("Set"))

#this is a huge hack for the lack of a copy constructor in Split
#but without it I'd have to manually type out a.SetFoo(b.GetFoo()) for every method under the sun
#and I make too many typos for that to be a good idea
def chain_mutations(a, b):
    #! DO NOT DO THIS IF THE TYPES DO NOT MATCH!
    assert type(a) == type(b)
    all_getters = getters(b)
    all_setters = setters(a)

    assert len(all_getters) == len(all_setters)

    for t in zip(all_setters, all_getters):
        f, g = t
        a.f(b.g())
