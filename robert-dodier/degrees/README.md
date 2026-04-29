`degrees` is an exploration of some ideas about representing degrees,
with optional minutes and seconds, in Maxima.

Here is the result of running the demonstration script `degrees.demo`.
Note that `degrees.lisp` and `degrees.mac` have already been loaded.

```
(%i2) demo ("degrees.demo") $

read and interpret /home/dodier/github/github-forks/maxima-packages/robert-dodier/degrees/degrees.demo

At the '_' prompt, type ';' and <enter> to proceed with the demonstration.
To abort the demonstration, type 'end;' or 'eof;' and then <enter>.
(%i3) "representation of degrees, minutes, and seconds"
_

(%i4) d1:degrees(45)
(%o4)                             45°
_

(%i5) d2:degrees(22,30)
(%o5)                           22° 30′
_

(%i6) d3:degrees(134,56,12)
(%o6)                         134° 56′ 12″
_

(%i7) grind(d1,d2,d3)
degrees(45)$
degrees(22,30)$
degrees(134,56,12)$
(%o7)                             done
_

(%i8) "arithmetic on degrees expressions"
_

(%i9) d1+d2
(%o9)                           67° 30′
_

(%i10) d1+d2+d3
(%o10)                        202° 26′ 12″
_

(%i11) 2*d3
(%o11)                        269° 52′ 24″
_

(%i12) "conversions from/to radians"
_

(%i13) degrees(from_radians(1))
                                  180
(%o13)                           (───)°
                                  %pi
_

(%i14) ev(%,numer)
(%o14)                     57.29577951308232°
_

(%i15) degrees(from_radians(1/9))
                                  20
(%o15)                           (───)°
                                  %pi
_

(%i16) radians(from_degrees(degrees(45/2)))
                                  %pi
(%o16)                            ───
                                   8
_

(%i17) radians(from_degrees(degrees(1,1,1)))
                                3661 %pi
(%o17)                          ────────
                                 648000
_

(%i18) "by default, degrees arguments are preserved in trig functions"
_

(%i19) L1:makelist(T(d1),T,[sin,cos,tan,csc,sec,cot])
(%o19) [sin(45°), cos(45°), tan(45°), csc(45°), sec(45°), cot(45°)]
_

(%i20) L2:makelist(T(d2),T,[sin,cos,tan,csc,sec,cot])
(%o20) [sin(22° 30′), cos(22° 30′), tan(22° 30′), csc(22° 30′), 
                                             sec(22° 30′), cot(22° 30′)]
_

(%i21) L3:makelist(T(d3),T,[sin,cos,tan,csc,sec,cot])
(%o21) [sin(134° 56′ 12″), cos(134° 56′ 12″), tan(134° 56′ 12″), 
                csc(134° 56′ 12″), sec(134° 56′ 12″), cot(134° 56′ 12″)]
_

(%i22) "explicit evaluation of degrees in trig functions"
_

(%i23) trig_with_degrees(L1)
                   1        1
(%o23)         [───────, ───────, 1, sqrt(2), sqrt(2), 1]
                sqrt(2)  sqrt(2)
_

(%i24) trig_with_degrees(L2)
            %pi       %pi       %pi       %pi       %pi       %pi
(%o24) [sin(───), cos(───), tan(───), csc(───), sec(───), cot(───)]
             8         8         8         8         8         8
_

(%i25) trig_with_degrees(L3)
            40481 %pi       40481 %pi       40481 %pi       40481 %pi
(%o25) [sin(─────────), cos(─────────), tan(─────────), csc(─────────), 
              54000           54000           54000           54000
                                             40481 %pi       40481 %pi
                                         sec(─────────), cot(─────────)]
                                               54000           54000
_

(%i26) "degrees with variable bits"
_

(%i27) e1:degrees(45+x)
(%o27)                         (x + 45)°
_

(%i28) e2:degrees(z,2*y)
(%o28)                         z° (2 y)′
_

(%i29) e3:degrees(z,15,t)
(%o29)                         z° 15′ t″
_

(%i30) sin(e1)
(%o30)                       sin((x + 45)°)
_

(%i31) cos(e2)
(%o31)                       cos(z° (2 y)′)
_

(%i32) tan(e3)
(%o32)                       tan(z° 15′ t″)
_

(%i33) trig_with_degrees(sin(e1))
                               %pi (x + 45)
(%o33)                     sin(────────────)
                                   180
_

(%i34) trig_with_degrees(cos(e2))
                                        y
                               %pi (z + ──)
                                        30
(%o34)                     cos(────────────)
                                   180
_

(%i35) trig_with_degrees(tan(e3))
                                      t     1
                            %pi (z + ──── + ─)
                                     3600   4
(%o35)                  tan(──────────────────)
                                   180
_

```
