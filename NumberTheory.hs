module NumberTheory
(next_pq_prime
,next_prime
,next_primitive_root
,rabin_miller
,fast_pow
,get_num_bits
,next_nbit_prime
) where

-- Fast powering modulo n for integers
fast_pow :: Integer -> Integer -> Integer -> Integer
fast_pow a 0 n = 1
fast_pow a b n = (a^(b `mod` 2)) * (fast_pow a (b `div` 2) n)^2 `mod` n

-- Extended version of Euclid's algorithm, given a and b, xeu returns
-- the triple (g, u, v) such that u*a + v*b = g = gcd(a,b).
-- See Hoffstein exercise 1.12 p 50. We use a helper function
-- xeuh to initialize the algorithm.
xeuh u g x 0 a b = (g, u, div (g-a*u) b)
xeuh u g x y a b = let
                     s = u - (div g y)*x
                     t = mod g y
                   in
                     xeuh x y s t a b

xeu :: Integer -> Integer -> (Integer, Integer, Integer)
xeu a b = xeuh 1 a 0 b a b

-- Calculate inverse modulo n using the extended euclid's algorithm
inverse :: Integer -> Integer -> Integer
inverse a n = let (g, u, v) = xeu a n in mod u n

{-
  For probabilistic primality testing we use Rabin-Miller which is
  state of the art. We need quite a few helper functions. The
  algorithm is described in Hoffstein, Table 3.2 p 127.
-}

-- Extract factors of two so n can be written n = a*2^b, the return
-- value is the pair (a, b).
twofactorsh n k = if n `mod` 2 == 1 then (n, k) else twofactorsh (n `div` 2) (k+1);
twofactors n = twofactorsh n 0;

wh n a q 0 = if (fast_pow a q n) == n-1 then False else True
wh n a q i = if (fast_pow a ((fast_pow 2 i n)*q) n) == n-1 then False else wh n a q (i-1)

witnessh n a q k = if fast_pow a q n == 1 then False else wh n a q k;

witness n a = let tf = twofactors (n-1) in witnessh n a (fst tf) (snd tf)

-- We use k witnesses, so the probability of being wrong less than 4^(-k).
-- Return True if n is prime, otherwise False
rabin_miller :: Integer -> Integer -> Bool
rabin_miller n 0 = True
rabin_miller n k = if witness n k then False else rabin_miller n (k-1)

{-
  Use Rabin-Miller to find a prime (according to k witnesses) by starting at
  an odd number n, and increasing it by two until we hit a prime. By noting
  that selecting a random integer uniqely defines a prime number that is the
  smallest prime number larger than or equal to the random integer we realize
  that if the starting point is random, the prime is random.
-}

next_primeh n = let k = 100
               in if rabin_miller n k then n else next_prime (n+2)

next_prime n = next_primeh $ oddify n

{-
  Search for an n-bit prime and give up if we can't find one
-}

-- pre-condition: n is odd
next_nbit_primeh n bits =
    let k = 100
        b = get_num_bits n
    in
        if b > bits then
            Nothing
        else
            if rabin_miller n k then
                Just n
            else
                next_nbit_primeh (n+2) bits

next_nbit_prime :: Integer -> Int -> Maybe Integer
next_nbit_prime n bits = next_nbit_primeh (oddify n) bits

{-
  We will also need the (quite common) primes of the form p=2q+1 where q
  is also prime.
-}
next_pq_primeh start = let q = next_prime start
                           p = 2*q+1
                       in if rabin_miller p 100 then
                           (p, q)
                       else
                           next_pq_primeh (mod (q+2) p)

next_pq_prime start = next_pq_primeh $ oddify start

-- If p,q are primes and p=2q+1, we have a nice condition that is only true
-- if a number g is a generator for Z_p*. See exercise 1.33 in Hoffstein.
primitive_q :: Integer -> Integer -> Integer -> Bool
primitive_q p q g = if mod g p /= 1
                    && mod g p /= p-1
                    && fast_pow g q p /= 1
                    then True else False

-- Find a primitive root given a start position n for the search and
-- primes p and q such that p=2q+1. n should be in {1, ..., p-1}.
next_primitive_root p q n = if primitive_q p q n then
                                n
                            else
                                next_primitive_root p q (mod (n+1) p)

-- Simple function to tell how many bits an integer has
get_num_bits :: Integer -> Int
get_num_bits n = if n == 0 then 0 else 1 + (get_num_bits (div n 2))

-- Turn any integer into the nearest greater odd integer
oddify :: Integer -> Integer
oddify x = if x `mod` 2 == 1 then x else x+1
