"""
04_simulation.py - When does post hoc leniency adjustment help, and when can it harm?
Monte Carlo study (1000 replicates per condition) calibrated to the 2024 BIOMED viva
(candidate SD ~12.9, assessor leniency SD ~6.4, residual SD ~10.6 percentage points).

Generating model for candidate i examined by assessor j:
    X_ij = mu + L_j + b_j * (T_i - mu) + e_ij,     T_i ~ N(mu, sd_T^2), L_j ~ N(0, sd_L^2),
    b_j  = assessor 'spread' (discrimination) multiplier, log b_j ~ N(0, sd_b^2), e_ij ~ N(0, sd_e^2)
Conditions: sd_L in {0,2,4,6,8,10}; candidates per assessor m in {5,8,10,15,20,30,40,60,80} (10 assessors);
            spread heterogeneity sd_b in {0, 0.2}; allocation random vs ability-related (assessor
            cohorts differ in mean true ability, SD = 0.35*sd_T, e.g. streaming by timetable).
Methods: raw; mean-shift (centre only); BiasFix (centre + spread, z-score equating);
         shrinkage mean-shift (random-intercept BLUP, method-of-moments variance estimate).
Outcomes: assessor-attributable error (RMS difference from the 'fair mark' an average assessor would give, T + e);
          % of students moved closer to / further from their fair mark; RMSE vs true score T;
          Spearman correlation with T; pass/fail (<50) misclassification.
"""
import numpy as np, pandas as pd, itertools, sys, os
from scipy.stats import rankdata
rng = np.random.default_rng(20241105)
base = sys.argv[1] if len(sys.argv) > 1 else "."
MU, SD_T, SD_E, A, R = 72.0, 12.9, 10.6, 10, 1000
CUT = 50.0


def spearman_rows(a, b):
    ra = np.apply_along_axis(rankdata, 1, a); rb = np.apply_along_axis(rankdata, 1, b)
    ra -= ra.mean(1, keepdims=True); rb -= rb.mean(1, keepdims=True)
    return (ra * rb).sum(1) / np.sqrt((ra**2).sum(1) * (rb**2).sum(1))


rows = []
for sd_L, m, sd_b, alloc in itertools.product([0, 2, 4, 6, 8, 10], [5, 8, 10, 15, 20, 30, 40, 60, 80], [0.0, 0.2], ["random", "ability-related"]):
    N = A * m
    g = np.repeat(np.arange(A), m)                                    # assessor of each candidate
    shift = rng.normal(0, 0.35 * SD_T, (R, A)) if alloc == "ability-related" else np.zeros((R, A))
    T = MU + shift[:, g] + rng.normal(0, SD_T, (R, N)) * (np.sqrt(1 - 0.35**2) if alloc == "ability-related" else 1)
    L = rng.normal(0, sd_L, (R, A)); b = np.exp(rng.normal(0, sd_b, (R, A)))
    e = rng.normal(0, SD_E, (R, N))
    X = np.clip(MU + L[:, g] + b[:, g] * (T - MU) + e, 0, 100)
    IDEAL = np.clip(T + e, 0, 100)          # score from a perfectly 'average' assessor (same residual)
    # group statistics
    oh = np.eye(A)[g]                                                  # (N, A)
    gm = X @ oh / m                                                    # (R, A) assessor means
    gv = ((X - gm[:, g])**2) @ oh / (m - 1)                            # assessor variances
    allm, alls = X.mean(1, keepdims=True), X.std(1, ddof=1, keepdims=True)
    ms = X - gm[:, g] + allm
    sdg = np.sqrt(gv[:, g]); bf = np.minimum(np.where(sdg > 0, (X - gm[:, g]) / np.where(sdg > 0, sdg, 1), 0) * alls + allm, 100)
    # shrinkage (BLUP of random intercept, method of moments)
    within = gv.mean(1, keepdims=True)
    tau2 = np.maximum(gm.var(1, ddof=1, keepdims=True) - within / m, 0)
    k = tau2 / (tau2 + within / m)
    sh = X - (k * (gm - allm))[:, g]
    for name, Y in [("raw", X), ("mean-shift", ms), ("BiasFix", bf), ("shrinkage", sh)]:
        rmse = np.sqrt(((Y - T)**2).mean(1))
        aerr = np.sqrt(((Y - IDEAL)**2).mean(1))      # assessor-attributable error
        closer = (np.abs(Y - IDEAL) < np.abs(X - IDEAL) - 1e-9).mean(1) * 100   # % students moved closer to fair mark
        further = (np.abs(Y - IDEAL) > np.abs(X - IDEAL) + 1e-9).mean(1) * 100
        rho = spearman_rows(Y, T)
        mis = ((Y < CUT) != (T < CUT)).mean(1)
        rows.append(dict(sd_L=sd_L, m=m, sd_b=sd_b, allocation=alloc, method=name,
                         rmse=rmse.mean(), assessor_error=aerr.mean(), pct_closer=closer.mean(), pct_further=further.mean(), rho=rho.mean(), misclass=mis.mean() * 100,
                         rmse_raw_ratio=np.nan))
df = pd.DataFrame(rows)
raw = df[df.method == "raw"].set_index(["sd_L", "m", "sd_b", "allocation"])
df["rmse_change_pct"] = [100 * (r.rmse / raw.loc[(r.sd_L, r.m, r.sd_b, r.allocation), "rmse"] - 1) for r in df.itertuples()]
df["rho_change"] = [r.rho - raw.loc[(r.sd_L, r.m, r.sd_b, r.allocation), "rho"] for r in df.itertuples()]
df["misclass_change"] = [r.misclass - raw.loc[(r.sd_L, r.m, r.sd_b, r.allocation), "misclass"] for r in df.itertuples()]
df.drop(columns="rmse_raw_ratio").to_csv(os.path.join(base, "out", "S_simulation_results.csv"), index=False)
for meth in ["BiasFix","shrinkage"]:
    print(meth,"assessor error change"); 
    t=df[(df.sd_b == 0.2) & (df.allocation == "random")]
    r=t[t.method=="raw"].set_index(["sd_L","m"]).assessor_error; x=t[t.method==meth].set_index(["sd_L","m"]).assessor_error
    print((x-r).unstack().round(2))
    print(meth,"% closer"); print(t[t.method==meth].pivot(index="sd_L",columns="m",values="pct_closer").round(0))
