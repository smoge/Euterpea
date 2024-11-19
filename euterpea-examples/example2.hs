import Euterpea

song = p1 :+: p2 :+: p1 :+: p3

p1 = r1 :+: r1 :+: r2 :+: r3
  where
    r1 = e 4 qn :+: e 4 qn :+: e 4 hn
    r2 = glue 4 qn [e, g, c, d]
    r3 = e 4 wn

repme n note
  | n == 1 = note
  | otherwise = note :+: repme (n - 1) note

glue o d [n] = n o d
glue o d (n : ns) = n o d :+: glue o d ns

p2 = r1 :+: r2 :+: r3 :+: r4
  where
    r1 = repme 4 $ f 4 qn
    r2 = f 4 qn :+: (repme 2 $ e 4 qn) :+: (repme 2 $ e 4 en)
    r3 = glue 4 qn [e, d, d, e]
    r4 = d 4 hn :+: g 4 qn :+: qnr

p3 = r1 :+: r2 :+: r3 :+: r4
  where
    r1 = repme 4 $ f 4 qn
    r2 = f 4 qn :+: (repme 2 $ e 4 qn) :+: (repme 2 $ e 4 en)
    r3 = glue 4 qn [g, g, f, d]
    r4 = c 4 wn