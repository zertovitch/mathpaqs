with Ada.Text_IO;                       use Ada.Text_IO;

with Ada.Numerics.Generic_Real_Arrays;
with Generic_Real_Linear_Equations;

procedure Test_Cholesky is

  subtype Real is Long_Float;
  type Integer_Vector is array (Integer range <>) of Integer;

  package RIO is new Float_IO(Real);

  package GRA is new Ada.Numerics.Generic_Real_Arrays(Real);
  package GRLE is new Generic_Real_Linear_Equations(Real, GRA, Integer_Vector);

  use GRA, GRLE, RIO;

  procedure Put(A: Real_Matrix) is
  begin
    for i in A'Range(1) loop
      for j in A'Range(2) loop
        Put(A(i,j), 4,2,0);
      end loop;
      New_Line;
    end loop;
  end Put;

  procedure Test(A: Real_Matrix) is
    L: constant Real_Matrix:= Cholesky_Decomposition(A);
    Z: constant Real_Matrix:= L * Transpose(L) - A;
    res: Real:= 0.0;
  begin
    Put_Line("**** Cholesky test A -> LLt. L is:");
    Put(L);
    Put_Line("Check LLt - A, should be close to 0 matrix");
    Put(Z);
    for i in Z'Range(1) loop
      for j in Z'Range(2) loop
        res:= res + abs Z(i,j);
      end loop;
    end loop;
    Put("This sum should be close to 0 :");
    Put(res);
    New_Line;
    New_Line;
  end Test;

  A33: constant Real_Matrix(1..3, 1..3):=
        ((1.0 , 0.5 , 0.25),
         (0.5 , 1.0 , 0.06),
         (0.25, 0.06, 1.0));

  A44: constant Real_Matrix(1..4, 1..4):=
        ((1.0, 1.0,  1.0,  1.0),
         (1.0, 5.0,  5.0,  5.0),
         (1.0, 5.0, 14.0, 14.0),
         (1.0, 5.0, 14.0, 15.0));

  APM2: constant Real_Matrix(1..17, 1..17):=
    (
     (1.00000000000,0.03010148042,0.13840312415,0.04784783222,0.17022082490,0.27206768325,0.54160154232,0.19633978721,0.17022082490,0.27206768325,0.24062644755,0.04784783222,0.04596970864,0.12255101501,0.14757005970,0.19633978721,0.03820685235),
     (0.03010148042,1.00000000000,0.02330965804,0.00985406754,0.05643885657,0.03644039558,0.02371979123,0.04460085926,0.05643885657,0.03644039558,0.05819617471,0.00985406754,0.00036357185,0.00915218375,0.02764705194,0.04460085926,0.03369165018),
     (0.13840312415,0.02330965804,1.00000000000,0.43829879304,0.06750744005,0.18276944974,0.20045121028,0.13983822579,0.06750744005,0.18276944974,0.50258029610,0.43829879304,0.32755610239,0.06986763301,0.04516282404,0.13983822579,0.03347501649),
     (0.04784783222,0.00985406754,0.43829879304,1.00000000000,0.01789958885,0.05340768987,0.06184940040,0.02545884916,0.01789958885,0.05340768987,0.28517295773,0.99900000000,0.46061156109,0.03039995323,0.01113468202,0.02545884916,0.01033737146),
     (0.17022082490,0.05643885657,0.06750744005,0.01789958885,1.00000000000,0.30821772856,0.14074639560,0.27767257106,0.99900000000,0.30821772856,0.24289306858,0.01789958885,0.02169030902,0.11364306478,0.45789976812,0.27767257106,0.20287680696),
     (0.27206768325,0.03644039558,0.18276944974,0.05340768987,0.30821772856,1.00000000000,0.37215071486,0.69938412341,0.30821772856,0.99900000000,0.56192703004,0.05340768987,0.05274224782,0.13721923598,0.15996500862,0.69938412341,0.10237843369),
     (0.54160154232,0.02371979123,0.20045121028,0.06184940040,0.14074639560,0.37215071486,1.00000000000,0.25223185658,0.14074639560,0.37215071486,0.34042548196,0.06184940040,0.06600438289,0.10530230517,0.07630957481,0.25223185658,0.03268913738),
     (0.19633978721,0.04460085926,0.13983822579,0.02545884916,0.27767257106,0.69938412341,0.25223185658,1.00000000000,0.27767257106,0.69938412341,0.66643078433,0.02545884916,0.03251774276,0.12222763963,0.16697115538,0.99900000000,0.12499603807),
     (0.17022082490,0.05643885657,0.06750744005,0.01789958885,0.99900000000,0.30821772856,0.14074639560,0.27767257106,1.00000000000,0.30821772856,0.24289306858,0.01789958885,0.02169030902,0.11364306478,0.45789976812,0.27767257106,0.20287680696),
     (0.27206768325,0.03644039558,0.18276944974,0.05340768987,0.30821772856,0.99900000000,0.37215071486,0.69938412341,0.30821772856,1.00000000000,0.56192703004,0.05340768987,0.05274224782,0.13721923598,0.15996500862,0.69938412341,0.10237843369),
     (0.24062644755,0.05819617471,0.50258029610,0.28517295773,0.24289306858,0.56192703004,0.34042548196,0.66643078433,0.24289306858,0.56192703004,1.00000000000,0.28517295773,0.19058294880,0.13315713390,0.16016824287,0.66643078433,0.12107078626),
     (0.04784783222,0.00985406754,0.43829879304,0.99900000000,0.01789958885,0.05340768987,0.06184940040,0.02545884916,0.01789958885,0.05340768987,0.28517295773,1.00000000000,0.46061156109,0.03039995323,0.01113468202,0.02545884916,0.01033737146),
     (0.04596970864,0.00036357185,0.32755610239,0.46061156109,0.02169030902,0.05274224782,0.06600438289,0.03251774276,0.02169030902,0.05274224782,0.19058294880,0.46061156109,1.00000000000,0.03091170434,0.01610897305,0.03251774276,0.00845511747),
     (0.12255101501,0.00915218375,0.06986763301,0.03039995323,0.11364306478,0.13721923598,0.10530230517,0.12222763963,0.11364306478,0.13721923598,0.13315713390,0.03039995323,0.03091170434,1.00000000000,0.15237220656,0.12222763963,0.02705297210),
     (0.14757005970,0.02764705194,0.04516282404,0.01113468202,0.45789976812,0.15996500862,0.07630957481,0.16697115538,0.45789976812,0.15996500862,0.16016824287,0.01113468202,0.01610897305,0.15237220656,1.00000000000,0.16697115538,0.35878601108),
     (0.19633978721,0.04460085926,0.13983822579,0.02545884916,0.27767257106,0.69938412341,0.25223185658,0.99900000000,0.27767257106,0.69938412341,0.66643078433,0.02545884916,0.03251774276,0.12222763963,0.16697115538,1.00000000000,0.12499603807),
     (0.03820685235,0.03369165018,0.03347501649,0.01033737146,0.20287680696,0.10237843369,0.03268913738,0.12499603807,0.20287680696,0.10237843369,0.12107078626,0.01033737146,0.00845511747,0.02705297210,0.35878601108,0.12499603807,1.00000000000)
    );

  APM3: constant Real_Matrix(1..9, 1..9):=
    (
     (1.00000000,0.14050025,0.14333523,0.35377655,0.55359194,0.29825265,0.37476163,0.87543863,0.12968781),
     (0.14050025,1.00000000,0.50114211,0.28233330,0.09549792,0.73701159,0.09079413,0.06944704,0.50431998),
     (0.14333523,0.50114211,1.00000000,0.19311799,0.08453452,0.30399522,0.39084725,0.06553651,0.09717668),
     (0.35377655,0.28233330,0.19311799,1.00000000,0.21538340,0.25868923,0.12907522,0.12944155,0.10207543),
     (0.55359194,0.09549792,0.08453452,0.21538340,1.00000000,0.09622969,0.12792649,0.21603171,0.10466015),
     (0.29825265,0.73701159,0.30399522,0.25868923,0.09622969,1.00000000,0.31371287,0.17817304,0.57867181),
     (0.37476163,0.09079413,0.39084725,0.12907522,0.12792649,0.31371287,1.00000000,0.19726087,0.09732786),
     (0.87543863,0.06944704,0.06553651,0.12944155,0.21603171,0.17817304,0.19726087,1.00000000,0.07705670),
     (0.12968781,0.50431998,0.09717668,0.10207543,0.10466015,0.57867181,0.09732786,0.07705670,1.00000000)
    );

  APM4: constant Real_Matrix(1..11, 1..11):=
    (
     (1.0,0.318783313,0.360117539,0.668389423,0.817084844,0.328267058,0.857146667,0.113362352,0.279713048,0.220941872,0.58522569),
     (0.318783313,1.0,0.19912467,0.297757125,0.482910721,0.18815579,0.315412781,0.083645692,0.627889754,0.125058374,0.241961514),
     (0.360117539,0.19912467,1.0,0.211581718,0.504456203,0.291871301,0.479549216,0.536236344,0.149848039,0.771433321,0.459226654),
     (0.668389423,0.297757125,0.211581718,1.0,0.58966064,0.250984032,0.525852185,0.094978672,0.282153489,0.126700801,0.442256325),
     (0.817084844,0.482910721,0.504456203,0.58966064,1.0,0.346156559,0.82890175,0.190492045,0.482910721,0.357096234,0.587073948),
     (0.328267058,0.18815579,0.291871301,0.250984032,0.346156559,1.0,0.385798913,0.216940596,0.153353335,0.223206125,0.7051084),
     (0.857146667,0.315412781,0.479549216,0.525852185,0.82890175,0.385798913,1.0,0.176658026,0.257499383,0.34655883,0.68599411),
     (0.113362352,0.083645692,0.536236344,0.094978672,0.190492045,0.216940596,0.176658026,1.0,0.064996543,0.590969335,0.270136942),
     (0.279713048,0.627889754,0.149848039,0.282153489,0.482910721,0.153353335,0.257499383,0.064996543,1.0,0.106564872,0.219436259),
     (0.220941872,0.125058374,0.771433321,0.126700801,0.357096234,0.223206125,0.34655883,0.590969335,0.106564872,1.0,0.336377923),
     (0.58522569,0.241961514,0.459226654,0.442256325,0.587073948,0.7051084,0.68599411,0.270136942,0.219436259,0.336377923,1.0)
    );

begin
  Test( A33 );
  Test( A44 );
  Test( APM2 );
  Test( APM3 );
  Test( APM4 );
  Put("Finished - press return");
  Skip_Line;
end Test_Cholesky;
