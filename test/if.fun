let name iff = fun t1 => fun g => fun t2 => if t1 then g else t2 endif in iff True False True end