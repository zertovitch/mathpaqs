-- Dormand-Prince
-- Very high precision method for solving Ordinary Differential Equations
--
-- Constantes du tableau Runge-Kutta de Dormand-Prince 7(8)
-- 4-Dec-1998
-- Source: Hairer - Norsett - Wanner, Solving ODE I, Springer

package Dormand_Prince_8 is

  -- a

  a21: constant:= 1.0 / 18.0;

  a31: constant:= 1.0 / 48.0;
  a32: constant:= 1.0 / 16.0;

  a41: constant:= 1.0 / 32.0;
  a43: constant:= 3.0 / 32.0;

  a51: constant:=   5.0 / 16.0;
  a53: constant:= -75.0 / 64.0;
  a54: constant:= -a53;

  a61: constant:= 3.0 / 80.0;
  a64: constant:= 3.0 / 16.0;
  a65: constant:= 3.0 / 20.0;

  a71: constant:=  29443841.0 /  614563906.0;
  a74: constant:=  77736538.0 /  692538347.0;
  a75: constant:= -28693883.0 / 1125000000.0;
  a76: constant:=  23124283.0 / 1800000000.0;

  a81: constant:=   16016141.0 /  946692911.0;
  a84: constant:=   61564180.0 /  158732637.0;
  a85: constant:=   22789713.0 /  633445777.0;
  a86: constant:=  545815736.0 / 2771057229.0;
  a87: constant:= -180193667.0 / 1043307555.0;

  a91: constant:=   39632708.0 /  573591083.0;
  a94: constant:= -433636366.0 /  683701615.0;
  a95: constant:= -421739975.0 / 2616292301.0;
  a96: constant:=  100302831.0 /  723423059.0;
  a97: constant:=  790204164.0 /  839813087.0;
  a98: constant:=  800635310.0 / 3783071287.0;

  a101: constant:=    246121993.0 /  1340847787.0;
  a104: constant:= -37695042795.0 / 15268766246.0;
  a105: constant:=   -309121744.0 /  1061227803.0;
  a106: constant:=    -12992083.0 /   490766935.0;
  a107: constant:=   6005943493.0 /  2108947869.0;
  a108: constant:=    393006217.0 /  1396673457.0;
  a109: constant:=    123872331.0 /  1001029789.0;

  a111:  constant:=  -1028468189.0 /   846180014.0;
  a114:  constant:=   8478235783.0 /   508512852.0;
  a115:  constant:=   1311729495.0 /  1432422823.0;
  a116:  constant:= -10304129995.0 /  1701304382.0;
  a117:  constant:= -48777925059.0 /  3047939560.0;
  a118:  constant:=  15336726248.0 /  1032824649.0;
  a119:  constant:= -45442868181.0 /  3398467696.0;
  a1110: constant:=   3065993473.0 /   597172653.0;

  a121:  constant:=    185892177.0 /   718116043.0;
  a124:  constant:=  -3185094517.0 /   667107341.0;
  a125:  constant:=   -477755414.0 /  1098053517.0;
  a126:  constant:=   -703635378.0 /   230739211.0;
  a127:  constant:=   5731566787.0 /  1027545527.0;
  a128:  constant:=   5232866602.0 /   850066563.0;
  a129:  constant:=  -4093664535.0 /   808688257.0;
  a1210: constant:=   3962137247.0 /  1805957418.0;
  a1211: constant:=     65686358.0 /   487910083.0;

  a131:  constant:=    403863854.0 /   491063109.0;
  a134:  constant:=  -5068492393.0 /   434740067.0;
  a135:  constant:=   -411421997.0 /   543043805.0;
  a136:  constant:=    652783627.0 /   914296604.0;
  a137:  constant:=  11173962825.0 /   925320556.0;
  a138:  constant:= -13158990841.0 /  6184727034.0;
  a139:  constant:=   3936647629.0 /  1978049680.0;
  a1310: constant:=   -160528059.0 /   685178525.0;
  a1311: constant:=    248638103.0 /  1413531060.0;

  -- b^ (8)

  b1:  constant:=    14005451.0 /  335480064.0;
  b6:  constant:=   -59238493.0 / 1068277825.0;
  b7:  constant:=   181606767.0 /  758867731.0;
  b8:  constant:=   561292985.0 /  797845732.0;
  b9:  constant:= -1041891430.0 / 1371343529.0;
  b10: constant:=   760417239.0 / 1151165299.0;
  b11: constant:=   118820643.0 /  751138087.0;
  b12: constant:=  -528747749.0 / 2220607170.0;
  b13: constant:=           1.0 /          4.0;

  -- b (7)

  bh1:  constant:=    13451932.0 /  455176623.0;
  bh6:  constant:=  -808719846.0 /  976000145.0;
  bh7:  constant:=  1757004468.0 / 5645159321.0;
  bh8:  constant:=   656045339.0 /  265891186.0;
  bh9:  constant:= -3867574721.0 / 1518517206.0;
  bh10: constant:=   465885868.0 /  322736535.0;
  bh11: constant:=    53011238.0 /  667516719.0;
  bh12: constant:=           2.0 /         45.0;

  -- c

  c2:   constant:=          1.0 /         18.0;
  c3:   constant:=          1.0 /         12.0;
  c4:   constant:=          1.0 /          8.0;
  c5:   constant:=          5.0 /         16.0;
  c6:   constant:=          3.0 /          8.0;
  c7:   constant:=         59.0 /        400.0;
  c8:   constant:=         93.0 /        200.0;
  c9:   constant:= 5490023248.0 / 9719169821.0;
  c10:  constant:=         13.0 /         20.0;
  c11:  constant:= 1201146811.0 / 1299019798.0;
  c12:  constant:=          1.0;
  c13:  constant:=          1.0;

end Dormand_Prince_8;
