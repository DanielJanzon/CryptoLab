
module CyclicGroup
(Group(..)
,Element(..)
,get_element
,op
,inverse
,pow
) where

data Group = Group { order :: Integer } deriving Show
data Element = Element Integer deriving Show

xeuh u g x 0 a b = (g, u, div (g-a*u) b)
xeuh u g x y a b = let
                     s = u - (div g y)*x
                     t = mod g y
                   in
                     xeuh x y s t a b
xeu a b = xeuh 1 a 0 b a b

inverse :: Group -> Element -> Element
inverse (Group n) (Element a) = let (g, u, _) = xeu a n in Element (mod u n)

get_element :: Group -> Integer -> Element
get_element (Group n) seed = Element (mod seed n)

op :: Group -> Element -> Element -> Element
op (Group n) (Element a) (Element b) = Element (mod (a*b) n)

pow :: Group -> Element -> Integer -> Element
pow (Group n) (Element a) 0 = Element 1
pow (Group n) (Element a) b = let Element rek = pow (Group n) (Element a) (b `div` 2) in
                              Element ((a^(b `mod` 2)) * (rek)^2 `mod` n)
