(function(args)
    lst = { a=function(arg) print("hello") end,
             b=(1+2)*3/4,
             [3+5]={ ["hello"]=("hi") },
         }
    lst[
    (function() return 0 end)()] = 1
end)("blah")

[[
Special lua string...
]]
