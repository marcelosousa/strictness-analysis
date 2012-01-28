let name factz = nfun fact x => if x == 0
                                then 1
                                else x * (fact (x - 1))
                                endif
in factz 3                     
end