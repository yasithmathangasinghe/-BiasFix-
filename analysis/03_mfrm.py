"""
03_mfrm.py - Many-facet Rasch (rating scale) model fitted by marginal maximum likelihood
Facets: candidate (random, N(0, sigma^2)), criterion/item (fixed), assessor (fixed), shared Andrich thresholds.
  log[P(k)/P(k-1)] = theta_n - delta_i - lambda_j - tau_k
Ratings recorded in half-marks are rounded half-up to whole marks (0-10) and unused categories collapsed.
Outputs: out/mfrm_scores.csv  (MFRM-adjusted station scores on the original % scale)
         out/mfrm_parameters.csv (assessor severities, item difficulties, thresholds, fit)
Author: analysis code accompanying the manuscript; Python 3.11, numpy, scipy.
"""
import sys, os, numpy as np, pandas as pd
from scipy.optimize import minimize
from scipy.special import logsumexp

base = sys.argv[1] if len(sys.argv) > 1 else "."
I = pd.read_csv(os.path.join(base, "data", "viva_items_deid.csv"))
E = pd.read_csv(os.path.join(base, "data", "viva_encounters_deid.csv"))
GH_z, GH_w = np.polynomial.hermite_e.hermegauss(41)          # probabilists' Hermite
GH_w = GH_w / GH_w.sum()

# scoring rules mapping criterion ratings (0-10) to the reported % score of one station/encounter
WEIGHTS = {"BIO24": {"content_knowledge": 3, "application": 6, "terminology": 1},
           "DEV24": {"content_knowledge": 3, "application": 6, "terminology": 1},
           "BIO23": {"specimen1": 2.5, "specimen2": 2.5, "specimen3": 2.5, "communication": 2.5}}


def fit_mfrm(d):
    """d: columns cand (int), item (int), rater (int), k (int category 0..K-1)"""
    N, NI, NR, K = d.cand.max() + 1, d.item.max() + 1, d.rater.max() + 1, d.k.max() + 1
    cand, item, rater, k = d.cand.values, d.item.values, d.rater.values, d.k.values

    def unpack(p):
        a = 0
        di = np.r_[p[a:a + NI - 1], -p[a:a + NI - 1].sum()]; a += NI - 1
        lj = np.r_[p[a:a + NR - 1], -p[a:a + NR - 1].sum()]; a += NR - 1
        tk = np.r_[p[a:a + K - 2], -p[a:a + K - 2].sum()]; a += K - 2      # tau_1..tau_{K-1}
        ls = p[a]
        return di, lj, tk, ls

    cats = np.arange(K)
    ge = (cats[None, :] >= np.arange(1, K)[:, None]).astype(float)       # (K-1, K): 1[m >= h]

    def nll(p, grad=True):
        di, lj, tk, ls = unpack(p); s = np.exp(ls)
        T = np.r_[0.0, np.cumsum(tk)]                                       # T_m
        eta = s * GH_z[None, :] - di[item][:, None] - lj[rater][:, None]    # (O, Q)
        num = eta[:, :, None] * cats[None, None, :] - T[None, None, :]      # (O, Q, K)
        lse = logsumexp(num, axis=2)
        pk = np.exp(num - lse[:, :, None])
        logp = np.take_along_axis(num, k[:, None, None].repeat(len(GH_z), 1), axis=2)[:, :, 0] - lse
        S = np.zeros((N, len(GH_z))); np.add.at(S, cand, logp)
        Lw = S + np.log(GH_w)[None, :]
        lL = logsumexp(Lw, axis=1)
        if not grad:
            return -lL.sum()
        post = np.exp(Lw - lL[:, None])                                     # (N, Q)
        pq = post[cand]                                                     # (O, Q)
        EK = (pk * cats[None, None, :]).sum(2)
        dEta = (k[:, None] - EK) * pq                                       # weighted d logP/d eta
        g_di = -np.bincount(item, dEta.sum(1), NI)
        g_lj = -np.bincount(rater, dEta.sum(1), NR)
        g_s = (dEta * GH_z[None, :]).sum() * s
        # thresholds: d logP / d tau_h = -1[k>=h] + P(K>=h)
        PgeH = np.einsum('oqk,hk->oqh', pk, ge)                             # (O,Q,K-1)
        kge = (k[:, None] >= np.arange(1, K)[None, :]).astype(float)        # (O,K-1)
        g_tk = (-(kge[:, None, :]) + PgeH) * pq[:, :, None]
        g_tk = g_tk.sum((0, 1))
        # chain rule for sum-to-zero parameterisation
        gf = np.r_[g_di[:-1] - g_di[-1], g_lj[:-1] - g_lj[-1], g_tk[:-1] - g_tk[-1], g_s]
        return -lL.sum(), -gf

    p0 = np.zeros((NI - 1) + (NR - 1) + (K - 2) + 1)
    # sensible starting thresholds: spread around 0
    r = minimize(lambda p: nll(p), p0, jac=True, method="L-BFGS-B", options={"maxiter": 2000})
    di, lj, tk, ls = unpack(r.x)
    # standard errors from numerical Hessian of the analytic gradient (free params)
    try:
        eps = 1e-4; H = np.zeros((len(r.x), len(r.x)))
        for a in range(len(r.x)):
            e = np.zeros_like(r.x); e[a] = eps
            H[:, a] = (nll(r.x + e)[1] - nll(r.x - e)[1]) / (2 * eps)
        cov = np.linalg.inv((H + H.T) / 2)
        se_free = np.sqrt(np.clip(np.diag(cov), 0, None))
    except Exception:
        se_free = np.full(len(r.x), np.nan)
    return dict(delta=di, lam=lj, tau=tk, sigma=np.exp(ls), loglik=-r.fun, conv=r.success, niter=r.nit,
                se_lam=np.r_[se_free[NI - 1:NI - 1 + NR - 1], np.nan], K=K)


def eap(obs_items, obs_raters, obs_k, P, sigma):
    """EAP ability from a subset of observations of one candidate"""
    T = np.r_[0.0, np.cumsum(P["tau"])]; cats = np.arange(P["K"])
    th = sigma * GH_z
    eta = th[None, :] - P["delta"][obs_items][:, None] - P["lam"][obs_raters][:, None]
    num = eta[:, :, None] * cats - T
    logp = np.take_along_axis(num, obs_k[:, None, None].repeat(len(th), 1), 2)[:, :, 0] - logsumexp(num, 2)
    lw = logp.sum(0) + np.log(GH_w)
    w = np.exp(lw - logsumexp(lw))
    return (w * th).sum()


def mle_theta(obs_items, obs_raters, obs_k, P):
    """Maximum-likelihood ability for a candidate-station (raw-score equation; extreme scores
    adjusted by 0.3 score points as in Facets). Unlike EAP, this is not shrunk toward the mean."""
    from scipy.optimize import brentq
    T = np.r_[0.0, np.cumsum(P["tau"])]; cats = np.arange(P["K"])
    dd = P["delta"][obs_items] + P["lam"][obs_raters]
    raw = obs_k.sum(); mx = (P["K"] - 1) * len(obs_k)
    raw = min(max(raw, 0.3), mx - 0.3)
    def f(t):
        num = (t - dd)[:, None] * cats - T
        p = np.exp(num - logsumexp(num, 1)[:, None])
        return (p * cats).sum() - raw
    return brentq(f, -15, 15)


def expected_rating(theta, d, lam, P, catval):
    T = np.r_[0.0, np.cumsum(P["tau"])]; cats = np.arange(P["K"])
    num = (theta - d - lam) * cats - T
    p = np.exp(num - logsumexp(num))
    return (p * catval).sum()


if __name__ == "__main__":
    rows, params = [], []
    for ex in ["BIO23", "BIO24", "DEV24"]:
        d = I[I.exam_id == ex].dropna(subset=["rating"]).copy()
        if ex == "DEV24":   # complete-case candidates (3 stations)
            keep = E[E.exam_id == "DEV24"].groupby("unit_id").size()
            d = d[d.unit_id.isin(keep[keep == 3].index)]
        d["crit"] = d["item"].str.replace(r"^S\d_", "", regex=True)
        d["r_int"] = np.floor(d.rating + 0.5).astype(int)                     # round half up
        used = np.sort(d.r_int.unique())
        cmap = {v: i for i, v in enumerate(used)}; catval = used.astype(float)  # category -> rating value
        d["k"] = d.r_int.map(cmap)
        d["cand"] = d.unit_id.astype("category").cat.codes
        d["itemc"] = d["item"].astype("category"); d["item_i"] = d.itemc.cat.codes
        d["rat"] = d.examiner_global.astype("category"); d["rater"] = d.rat.cat.codes
        fit = fit_mfrm(d[["cand", "item_i", "rater", "k"]].rename(columns={"item_i": "item"}))
        items = list(d.itemc.cat.categories); raters = list(d.rat.cat.categories)
        print(ex, "obs", len(d), "cands", d.cand.nunique(), "items", len(items), "raters", len(raters), "K", fit["K"],
              "sigma", round(fit["sigma"], 3), "conv", fit["conv"], "iter", fit["niter"], "logLik", round(fit["loglik"], 1))
        for j, rname in enumerate(raters):
            params.append(dict(exam_id=ex, facet="assessor", element=rname, measure=fit["lam"][j], se=fit["se_lam"][j]))
        for i, iname in enumerate(items):
            params.append(dict(exam_id=ex, facet="item", element=iname, measure=fit["delta"][i], se=np.nan))
        for h, t in enumerate(fit["tau"]):
            params.append(dict(exam_id=ex, facet="threshold", element=f"tau{h+1}", measure=t, se=np.nan))
        params.append(dict(exam_id=ex, facet="sigma", element="candidate SD (logits)", measure=fit["sigma"], se=np.nan))
        # ----- adjusted station scores -----
        ii = {n: i for i, n in enumerate(items)}; rr = {n: j for j, n in enumerate(raters)}
        W = WEIGHTS[ex]
        lam_ref = np.average(fit["lam"][d.rater.values])      # encounter-weighted average assessor = reference
        # ---- fit statistics (infit/outfit mean-squares) with candidate EAP from all their data ----
        cats_ = np.arange(fit["K"]); Tq = np.r_[0.0, np.cumsum(fit["tau"])]
        th_all = {c: eap(g.item_i.values, g.rater.values, g.k.values, fit, fit["sigma"]) for c, g in d.groupby("cand")}
        eta_o = np.array([th_all[c] for c in d.cand]) - fit["delta"][d.item_i.values] - fit["lam"][d.rater.values]
        num = eta_o[:, None] * cats_ - Tq; pr = np.exp(num - logsumexp(num, 1)[:, None])
        Ek = (pr * cats_).sum(1); Vk = (pr * cats_**2).sum(1) - Ek**2
        d["res2"] = (d.k.values - Ek)**2; d["V"] = Vk; d["z2"] = d.res2 / Vk
        fitst = d.groupby("examiner_global").agg(infit=("res2", "sum"), V=("V", "sum"), outfit=("z2", "mean"), n_obs=("k", "size"))
        fitst["infit"] = fitst.infit / fitst.V
        var_l = np.var(fit["lam"], ddof=1); mse = np.nanmean(fit["se_lam"]**2)
        params.append(dict(exam_id=ex, facet="assessor_separation_reliability", element="R", measure=max(0, (var_l - mse) / var_l), se=np.nan))
        for rname, rw in fitst.iterrows():
            params.append(dict(exam_id=ex, facet="assessor_infit", element=rname, measure=rw.infit, se=np.nan))
            params.append(dict(exam_id=ex, facet="assessor_outfit", element=rname, measure=rw.outfit, se=np.nan))
        for (u, st), g in d.groupby(["unit_id", "station"]):
            th = mle_theta(g.item_i.values, g.rater.values, g.k.values, fit)
            rj = rr[g.examiner_global.iloc[0]]
            exp_obs = sum(W[c] * expected_rating(th, fit["delta"][ii[it]], fit["lam"][rj], fit, catval) for it, c in zip(g["item"], g.crit))
            exp_fair = sum(W[c] * expected_rating(th, fit["delta"][ii[it]], lam_ref, fit, catval) for it, c in zip(g["item"], g.crit))
            obs = E[(E.exam_id == ex) & (E.unit_id == u) & (E.station == st)].score_pct.iloc[0]
            rows.append(dict(exam_id=ex, unit_id=u, station=st, theta_mle_station=th, observed=obs,
                             fair_expected=exp_fair, mfrm=min(obs - (exp_obs - exp_fair), 100.0)))
    out = os.path.join(base, "out"); os.makedirs(out, exist_ok=True)
    pd.DataFrame(rows).to_csv(os.path.join(out, "mfrm_scores.csv"), index=False)
    pd.DataFrame(params).to_csv(os.path.join(out, "mfrm_parameters.csv"), index=False)
    print("saved")
