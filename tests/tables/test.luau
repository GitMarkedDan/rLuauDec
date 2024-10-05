empty_table = {}
local bruh = tostring(1)
local t = {bruh, "hi", nil, true}
t[5] = false
print(bruh)
const_table = t
-- we'll get to that when we're ready.
list_table = {empty_table, const_table--[[ , tostring(1) ]]}
