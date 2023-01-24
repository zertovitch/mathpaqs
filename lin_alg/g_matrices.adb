-------------------------------------------------------------------------------
--  File: gmatrice.adb; see specification (gmatrice.ads)
-------------------------------------------------------------------------------

package body G_Matrices is

  function Id (order : Positive) return Matrix is
    I : Matrix (1 .. order, 1 .. order);
  begin
    for k in I'Range (1) loop
      for l in I'Range (2) loop
        if k = l then I (k, l) := one; else I (k, l) := zero; end if;
      end loop;
    end loop;
    return I;
  end Id;

  function "*" (l : Field_Element; A : Matrix) return Matrix is
    l_A : Matrix (A'Range (1), A'Range (2));
  begin
    for i in A'Range (1) loop
      for j in A'Range (2) loop
        l_A (i, j) := l * A (i, j);
      end loop;
    end loop;
    return l_A;
  end "*";

  function Transpose (A : Matrix) return Matrix is
    B : Matrix (A'Range (2), A'Range (1));
  begin
    for i in A'Range (1) loop
      for j in A'Range (2) loop
        B (j, i) := A (i, j);
      end loop;
    end loop;
    return B;
  end Transpose;

  function "*" (l : Field_Element; v : Vector) return Vector is
    r : Vector (v'Range);
  begin
    for i in v'Range loop r (i) := v (i) * l; end loop;
    return r;
  end "*";

  function "-" (a : Vector) return Vector is
    r : Vector (a'Range);
  begin
    for i in a'Range loop r (i) := -a (i); end loop;
    return r;
  end "-";

  function "+" (a, b : Vector) return Vector is
    r : Vector (a'Range);
  begin
    for i in a'Range loop r (i) := a (i) + b (i); end loop;
    return r;
  end "+";

  function "-" (a, b : Vector) return Vector is
    r : Vector (a'Range);
  begin
    for i in a'Range loop r (i) := a (i) - b (i); end loop;
    return r;
  end "-";

  function "*" (a, b : Vector) return Field_Element is
    r : Field_Element := zero;
  begin
    for i in a'Range loop r := r + a (i) * b (i); end loop;
    return r;
  end "*";

  function Norm (a : Vector) return Field_Element is
    r : Field_Element := zero;
  begin
    for i in a'Range loop r := r + a (i) * a (i); end loop;
    return Sqrt (r);
  end Norm;

  function Square_Norm (a : Vector) return Field_Element is
    r : Field_Element := zero;
  begin
    for i in a'Range loop r := r + a (i) * a (i); end loop;
    return r;
  end Square_Norm;

  function Distance (a, b : Vector) return Field_Element is
    r : Field_Element := zero; ab : Field_Element;
  begin
    for i in a'Range loop ab := a (i) - b (i); r := r + ab * ab; end loop;
    return Sqrt (r);
  end Distance;

  function Square_Distance (a, b : Vector) return Field_Element is
    r  : Field_Element := zero;
    ab : Field_Element;
  begin
    for i in a'Range loop ab := a (i) - b (i); r := r + ab * ab; end loop;
    return r;
  end Square_Distance;

  function "*" (A, B : Matrix) return Matrix is
    r : Field_Element;
    AB : Matrix (A'Range (1), B'Range (2));
  begin
    if A'Length (2) /= B'Length (1) then raise Constraint_Error; end if;
    for i in A'Range (1) loop
      for j in B'Range (2) loop
        r := zero;
        for k in B'Range (1) loop
          r := r + A (i, k - B'First (1) + A'First (2)) * B (k, j);
        end loop;
        AB (i, j) := r;
      end loop;
    end loop;
    return AB;
  end "*";

  function "+" (A, B : Matrix) return Matrix is
    ApB : Matrix (A'Range (1), A'Range (2));
  begin
    for i in A'Range (1) loop
      for j in A'Range (2) loop
        ApB (i, j) := A (i, j) + B (i, j);
      end loop;
    end loop;
    return ApB;
  end "+";

  function "-" (A, B : Matrix) return Matrix is
    AmB : Matrix (A'Range (1), A'Range (2));
  begin
    for i in A'Range (1) loop
      for j in A'Range (2) loop
        AmB (i, j) := A (i, j) - B (i, j);
      end loop;
    end loop;
    return AmB;
  end "-";

  function "*" (A : Matrix; x : Vector) return Vector is
    r : Field_Element;
    Ax : Vector (A'Range (1));
  begin
    if A'Length (2) /= x'Length then raise Constraint_Error; end if;
    for i in A'Range (1) loop
      r := zero;
      for j in x'Range loop
        r := r + A (i, j - x'First + A'First (2)) * x (j);
      end loop;
      Ax (i) := r;
    end loop;
    return Ax;
  end "*";

end G_Matrices;
