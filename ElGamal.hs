module ElGamal where

import qualified CyclicGroup as CG

encrypt (CG.Group n) msg g t pub = 
    let pow = CG.pow (CG.Group n)
        op  = CG.op (CG.Group n)
    in  (pow g t, op msg (pow pub t))

decrypt (CG.Group n) (c1, c2) priv =
    let pow = CG.pow (CG.Group n)
        op  = CG.op (CG.Group n)
        inverse = CG.inverse (CG.Group n)
    in  op c2 (pow (inverse c1) priv)

