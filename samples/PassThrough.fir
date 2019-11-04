circuit PassThrough :
  module PassThrough :
    input clock : Clock
    input a : SInt<8>
    input b : SInt<8>
    output c: SInt<9>
    output d: SInt<10>
  
    reg r : SInt<9>, clock
    r <= add(a, b)
    c <= add(a, b)
    inst adder of Add
    adder.r <= r
    adder.a <= a
    d <= adder.d
  
  module Add :
    input r : SInt<9>
    input a : SInt<8>
    output d : SInt<10>
    wire e : SInt<10>
  
    e <= add(r, a)
    d <= neg(e)
