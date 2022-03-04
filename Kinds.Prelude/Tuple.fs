namespace Prelude

module Tuple =
    let uncurry (t1, t2) = fun f -> f t1 t2
    let uncurry3 (t1, t2, t3) = fun f -> f t1 t2 t3
    let uncurry4 (t1, t2, t3, t4) = fun f -> f t1 t2 t3 t4
    let uncurry5 (t1, t2, t3, t4, t5) = fun f -> f t1 t2 t3 t4 t5
    let uncurry6 (t1, t2, t3, t4, t5, t6) = fun f -> f t1 t2 t3 t4 t5 t6
    let uncurry7 (t1, t2, t3, t4, t5, t6, t7) = fun f -> f t1 t2 t3 t4 t5 t6 t7
    let uncurry8 (t1, t2, t3, t4, t5, t6, t7, t8) = fun f -> f t1 t2 t3 t4 t5 t6 t7 t8
    let uncurry9 (t1, t2, t3, t4, t5, t6, t7, t8, t9) = fun f -> f t1 t2 t3 t4 t5 t6 t7 t8 t9
    let uncurry10 (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) = fun f -> f t1 t2 t3 t4 t5 t6 t7 t8 t9 t10

    let curry t1 t2 = fun f -> f (t1,t2)
    let curry3 t1 t2 t3 = fun f -> f (t1,t2,t3)
    let curry4 t1 t2 t3 t4 = fun f -> f (t1,t2,t3,t4)
    let curry5 t1 t2 t3 t4 t5 = fun f -> f (t1,t2,t3,t4,t5)
    let curry6 t1 t2 t3 t4 t5 t6 = fun f -> f (t1,t2,t3,t4,t5,t6)
    let curry7 t1 t2 t3 t4 t5 t6 t7 = fun f -> f (t1,t2,t3,t4,t5,t6,t7)
    let curry8 t1 t2 t3 t4 t5 t6 t7 t8 = fun f -> f (t1,t2,t3,t4,t5,t6,t7,t8)
    let curry9 t1 t2 t3 t4 t5 t6 t7 t8 t9 = fun f -> f (t1,t2,t3,t4,t5,t6,t7,t8,t9)
    let curry10 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 = fun f -> f (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10)

    let tuple t1 t2 = (t1,t2)
    let tuple3 t1 t2 t3 = (t1,t2,t3)
    let tuple4 t1 t2 t3 t4 = (t1,t2,t3,t4)
    let tuple5 t1 t2 t3 t4 t5 = (t1,t2,t3,t4,t5)
    let tuple6 t1 t2 t3 t4 t5 t6 = (t1,t2,t3,t4,t5,t6)
    let tuple7 t1 t2 t3 t4 t5 t6 t7 = (t1,t2,t3,t4,t5,t6,t7)
    let tuple8 t1 t2 t3 t4 t5 t6 t7 t8 = (t1,t2,t3,t4,t5,t6,t7,t8)
    let tuple9 t1 t2 t3 t4 t5 t6 t7 t8 t9 = (t1,t2,t3,t4,t5,t6,t7,t8,t9)
    let tuple10 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 = (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10)
    
    let untuple (t1, t2) =  t1 t2
    let untuple3 (t1, t2, t3) =  t1 t2 t3
    let untuple4 (t1, t2, t3, t4) =  t1 t2 t3 t4
    let untuple5 (t1, t2, t3, t4, t5) =  t1 t2 t3 t4 t5
    let untuple6 (t1, t2, t3, t4, t5, t6) =  t1 t2 t3 t4 t5 t6
    let untuple7 (t1, t2, t3, t4, t5, t6, t7) =  t1 t2 t3 t4 t5 t6 t7
    let untuple8 (t1, t2, t3, t4, t5, t6, t7, t8) =  t1 t2 t3 t4 t5 t6 t7 t8
    let untuple9 (t1, t2, t3, t4, t5, t6, t7, t8, t9) =  t1 t2 t3 t4 t5 t6 t7 t8 t9
    let untuple10 (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) =  t1 t2 t3 t4 t5 t6 t7 t8 t9 t10