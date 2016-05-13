counter = 0

request = function()
   path = "/hello/" .. counter
   counter = counter + 1
   return wrk.format(nil, path)
end
