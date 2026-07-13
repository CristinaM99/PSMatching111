/* Unmatched treatment-effect estimation from data_generation.sas
   (the tab_unmatchedB block, lines 235-248): the center 0-vs-1 subcohort
   (sim01) is fit with PROC LOGISTIC, center reference-coded against '0',
   and the odds ratio is captured with `ods output OddsRatios=`. This is
   the unmatched OR the paper compares against its matched estimates.
   The input is a small reproducible draw from the paper's own DGP
   (fixed CALL STREAMINIT), so the estimate is stable. The PROC LOGISTIC
   call and its ODS capture are unchanged from the source. */
data simul1;
    call streaminit(20260313);
    pA = 0.25; alpha = pA/(1-pA); pBC = 0.35;
    do ID_pz=1 to 300;
        center = rand("Table",1/3,1/3,1/3)-1;
        if center = 0 then beta_X = 1;
        else beta_X = (pBC/(1-pBC))/(pA/(1-pA));
        C1 = rand("Normal",0.2*center,1.5);
        C2 = rand("Normal",0.3*center,1.5);
        D1 = rand("Bernoulli",0.3);
        lin_comb = log(alpha) + log(beta_X) + log(1.1)*C1 + log(1.3)*C2 + log(1.9)*D1;
        prob_subj = 1/(1+exp(-lin_comb));
        Y = rand("Bernoulli",prob_subj);
        output;
    end;
    keep ID_pz center Y beta_X C1 C2 D1;
run;

data sim01;
set simul1;
if center ne 2;
run;

proc logistic data=sim01;
class center(ref='0')/ param=ref;
model Y(Event='1')= center / link=logit;
ods output OddsRatios=OR_unmatched01;
run;

data OR_unmatched01;
set OR_unmatched01;
keep oddsratioest;
rename oddsratioest=OR_unmatched01;
run;

proc print data=OR_unmatched01;
title "Unmatched odds ratio, center 1 vs 0";
run;
