-- f คือ  function ที่รับตัวแปรมา 1 ตัว และ คืนค่าไป 1 ตัว (a -> b) -> b
g :: (a -> b) -> b
g = s(k(s i)) k
-- ลอง proof
-- g x f = s(k(s i)) k x f
--       = k(s i) x (k x) f
--       = s i (k x) f
--       = i f ((k x) f)
--       = i f x
--       = f x

-- ลอง h (-) 5 2 จะได้ -3
h :: (a -> b -> c) -> b -> a -> c
h = s(s(k(s(k s)k))s) (k k)
-- proof ไม่ได้


