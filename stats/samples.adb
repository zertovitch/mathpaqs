with Ada.Exceptions;                    use Ada.Exceptions;

with Ada.Numerics.Generic_Elementary_Functions;

package body Samples is

  package REF is new Ada.Numerics.Generic_Elementary_Functions(Real);
  use REF;

  function Almost_zero(x: Real) return Boolean is
  begin
    return  abs x <= Real'Base'Model_Small;
  end Almost_zero;

  procedure Initialize(
    s       : out Sample;
    min, max: in  Real
  )
  is
    eps: constant:= 0.001;
    -- little contraction to make value of s.max
    -- land into bin n-1
  begin
    s.histogram:= (others => 0);
    s.min:= min;
    s.max:= max;
    s.total_occurences:= 0;
    s.bins_r:= Real(s.bins);
    s.bins_inv:= 1.0 / s.bins_r;
    s.factor:= (s.bins_r - eps) / (max-min);
    s.sum:= 0.0;
    s.sum_sq:= 0.0;
    s.initialized:= True;
  end Initialize;

  procedure Add_occurence(s: in out Sample; value: Real) is
    pos: Integer;
  begin
    if not s.initialized then
      raise sample_not_initialized;
    end if;
    s.sum:= s.sum + value;
    s.sum_sq:= s.sum_sq + value*value;
    pos:= 1 + Integer(Real'Floor(s.factor * (value - s.min)));
    if pos in s.histogram'Range then
      s.histogram(pos):= s.histogram(pos) + 1;
      s.total_occurences:= s.total_occurences + 1;
    else
      Raise_Exception
       (value_out_of_sample_range'Identity,
        "Value = " & Real'Image(value) &
        "; Sample.min = " & Real'Image(s.min) &
        "; Sample.max = " & Real'Image(s.max) &
        "; pos = " & Integer'Image(pos) &
        "; histogram'First = " & Integer'Image(s.histogram'First) &
        "; histogram'Last = " & Integer'Image(s.histogram'Last)
       );
    end if;
  end Add_occurence;

  procedure Get_measures(
    s: in     Sample;
    m: in out Measure -- "in" are the quantile levels (m.level)
  )
  is
    f, value, samples, sxi, mu, s2,
    inv_n, inv_nm1, n, ql, new_ql, prob_bigger: Real;
    q_idx: Integer:= m.level'First;
    cumul_samples, new_cumul_samples: Natural;
    truncated_mu: array(s.histogram'Range) of Real;
    sub_histo_idx: Real range 0.0..1.0:= 0.0;
  begin
    if s.total_occurences = 0 then
      raise no_occurence;
    end if;
    n:= Real(s.total_occurences);
    inv_n  := 1.0 / n;
    inv_nm1:= 1.0 / Real(s.total_occurences - 1);
    f:= (s.max-s.min) * s.bins_inv;
    ql:= m.level(q_idx) * n;
    if ql < 0.0 then
      raise Quantile_level_negative;
    end if;
    -- Sum for x >= x_i, x P(X=x)
    sxi:= 0.0;
    for i in reverse s.histogram'Range loop
      value:= s.min + f * Real(i);
      samples:= Real(s.histogram(i)); -- # of points for this value
      sxi:= sxi + samples * value;
      truncated_mu(i):= inv_n * sxi;
    end loop;
    --
    cumul_samples:= 0;
    for i in s.histogram'Range loop
      value:= s.min + f * Real(i);
      samples:= Real(s.histogram(i)); -- # of points for this value
      new_cumul_samples:= cumul_samples + s.histogram(i);
      while q_idx <= m.level'Last and then ql <= Real(new_cumul_samples) loop
        -- With the cumulative samples, we got past at least one quantile level
        if s.histogram(i) = 0 then
          raise Unexpected_case;
        end if;
        -- Linear interpolation, for regaining a bit of the precision lost
        -- by having histogram points instead of all exact values of the
        -- statistic.
        -- Rationale: the quantile level * number of observations (ql)
        -- can be in the middle of an histogram count.
        -- Contra: with discrete probabilities, this technique shows counter-
        -- intuitive results.
        if use_sub_histogram_index then
          sub_histo_idx:= (ql - Real(cumul_samples)) / Real(s.histogram(i));
        end if;
        m.VaR(q_idx):= s.min + f * (Real(i-1) + sub_histo_idx);
        -- TailVaR(q) = E(X|X>=VaR(q)) = E_Q(X)
        --
        -- Probability measure Q is defined by
        -- Q(A) = P(A | X>=VaR(q)) = P(A and X>=VaR(q)) / P(X>=VaR(q))
        prob_bigger:= (1.0 - m.level(q_idx));
        if Almost_zero(prob_bigger) then
          -- q = 100%, {X>=VaR(q)} is empty, then TailVaR is undefined
          m.TailVar(q_idx):= 0.0;
        else
          if i = s.histogram'First then
            m.TailVar(q_idx):= truncated_mu(i) / prob_bigger;
          else -- Extra precision, like for VaR
            m.TailVar(q_idx):=
              (truncated_mu(i-1) * (1.0-sub_histo_idx) +
               truncated_mu(i)   * sub_histo_idx
              )
              / prob_bigger;
          end if;
        end if;
        q_idx:= q_idx + 1;
        if q_idx <= m.level'Last then
          new_ql:= m.level(q_idx) * n;
          if new_ql < ql then
            raise Quantile_levels_not_ascending;
          end if;
          ql:= new_ql;
        end if;
      end loop;
      cumul_samples:= new_cumul_samples;
    end loop;
    mu:= inv_n * s.sum;
    s2:= inv_nm1 * (s.sum_sq - n * mu*mu); -- unbiased estimator for variance
    if s2 < 0.0 then
      -- cure small numerical inaccuracies in case of zero variance
      -- i.e. all value in the same bin, e.g. a constant random variable
      s2:= 0.0;
    end if;
    m.mean:= mu;
    m.std_dev:= Sqrt(s2);
    m.stat_err:= Sqrt(s2 * inv_n);
  end Get_measures;

end Samples;
