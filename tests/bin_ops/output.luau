local var1 = 1
one = var1
var1 = 2
two = var1
var1 = 3
three = var1
var1 = 4
four = var1
var1 = 5
five = var1
local var2 = one
local var3 = two
local var4 = three
local var5 = var3 * var4
local var6 = var2 + var5
var5 = four
var3 = five
var2 = var5 / var3
var1 = var6 - var2
test = var1
var3 = tostring
var4 = test
local var7 = var3(var4)
var6 = var7
var2 = " hi"
var5 = " there"
var1 = var6 .. var2 .. var5
test2 = var1
var1 = print
var6 = test
var1(var6)
local var8 = print
local var9 = test2
var8(var9)
return
