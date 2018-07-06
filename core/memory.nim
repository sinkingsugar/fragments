# memory utils

template alloc*(objtype: typedesc): ptr objtype = cast[ptr objtype](alloc(sizeof(objtype)))

template alloc0*(objtype: typedesc): ptr objtype = cast[ptr objtype](alloc0(sizeof(objtype)))
