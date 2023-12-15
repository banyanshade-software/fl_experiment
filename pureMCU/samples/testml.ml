
let rec pidctrl par st measval dt =
    let (kp,ki,kd) = par in
    let (v, sum, last, boost) = st in
    let err = v -. measval in
    let nsum = sum +. err in
    let rec maxval x = (if x=0.0 then (0.0) else (if x>0.0 then 100.0 else -100.0)) in
    let res = (if boost <= 0 then (kp *. err +. ki *. nsum +. (kd *. (measval -. last))/. dt) else (maxval v)) in
    let nboost = (if boost > 0 then (boost - 1) else 0) in
    let nst = (v,nsum,measval,nboost) in
    (nst,res) in
let testpar = (0.2, 0.1, 0.01) in
let st0 = (10.0, 0.0, 0.0, 0) in

let (st1,nv1) = pidctrl testpar st0 0. 0.01 in
let (st2,nv2) = pidctrl testpar st1 1. 0.01 in
let (st3,nv3) = pidctrl testpar st2 2. 0.01 in
let (st4,nv4) = pidctrl testpar st3 3. 0.01 in
let (st5,nv5) = pidctrl testpar st4 5. 0.01 in
let (st6,nv6) = pidctrl testpar st5 10. 0.01 in
let (st7,nv7) = pidctrl testpar st6 14. 0.01 in
let (st8,nv8) = pidctrl testpar st7 15. 0.01 in
print_float nv8
