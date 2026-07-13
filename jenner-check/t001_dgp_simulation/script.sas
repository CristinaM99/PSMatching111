/* Data generating process from data_generation.sas (%simulation macro,
   the data step that builds simul.simul&i). Adapted to run standalone:
   the LIBNAME target becomes WORK, one dataset is generated, and a fixed
   seed (CALL STREAMINIT) makes the draw reproducible. The modeling logic
   -- the RRcontrols/RRtreated odds-ratio structure in beta_X, the five
   continuous and five dichotomous confounders, the interaction terms, and
   the hand-assembled log-linear outcome predictor -- is unchanged from the
   paper. */
data simul1;
    call streaminit(20260313);
    rand_pA = rand("Bernoulli",0.5);
    if rand_pA = 0 then pA = 0.1;
    else if rand_pA = 1 then pA = 0.25;
    alpha = pA/(1-pA);
    rand_RD = rand("Bernoulli",0.5);
    if rand_RD = 0 then RD = 0.05;
    else if rand_RD = 1 then RD = 0.1;
    pBC = pA + RD;
    rand_sd = rand("Bernoulli",0.5);
    if rand_sd = 0 then sd = 0.75;
    else if rand_sd = 1 then sd = 1.5;
    rand_beta_IC = rand("Table",1/3,1/3,1/3);
    if rand_beta_IC = 1 then beta_IC = 1;
    else if rand_beta_IC = 2 then beta_IC = 1.5;
    else if rand_beta_IC = 3 then beta_IC = 2;
    rand_beta_ID = rand("Table",1/3,1/3,1/3);
    if rand_beta_ID = 1 then beta_ID = 1;
    else if rand_beta_ID = 2 then beta_ID = 1.5;
    else if rand_beta_ID = 3 then beta_ID = 2;
    rand_beta_U = rand("Table",1/3,1/3,1/3);
    if rand_beta_U = 1 then beta_U = 1;
    else if rand_beta_U = 2 then beta_U = 1.5;
    else if rand_beta_U = 3 then beta_U = 2;
    do ID_pz=1 to 300;
        center = rand("Table",1/3,1/3,1/3)-1;
        if center = 0 then do;
            beta_X = 1; *RRtreated/RRtreated;
        end;
        else if center = 1 or center = 2 then do;
            beta_X = (pBC/(1-pBC))/(pA/(1-pA)); *RRcontrols/RRtreated = ORcontrolsVStreated;
        end;
        C1 = rand("Normal",0.2*center,sd);
        C2 = rand("Normal",0.3*center,sd);
        C3 = rand("Normal",0.4*center,sd);
        C4 = rand("Normal",0.5*center,sd);
        C5 = rand("Normal",0.6*center,sd);
        if center = 0 then do;
            D1 = rand("Bernoulli",0.168);
            D2 = rand("Bernoulli",0.331);
            D3 = rand("Bernoulli",0.492);
            D4 = rand("Bernoulli",0.642);
            D5 = rand("Bernoulli",0.776);
        end;
        else if center = 1 then do;
            D1 = rand("Bernoulli",0.1);
            D2 = rand("Bernoulli",0.2);
            D3 = rand("Bernoulli",0.3);
            D4 = rand("Bernoulli",0.4);
            D5 = rand("Bernoulli",0.5);
        end;
        else if center = 2 then do;
            D1 = rand("Bernoulli",0.05);
            D2 = rand("Bernoulli",0.15);
            D3 = rand("Bernoulli",0.25);
            D4 = rand("Bernoulli",0.35);
            D5 = rand("Bernoulli",0.45);
        end;
        IC = C3*log(beta_X);
        ID = D3*log(beta_X);
        U = rand("Normal",0.4*center,1);
        lin_comb = log(alpha) + log(beta_X) + log(1.1)*C1 + log(1.3)*C2 + log(1.5)*C3 + log(1.7)*C4 + log(1.9)*C5 +
            log(1.9)*D1 + log(1.7)*D2 + log(1.5)*D3 + log(1.3)*D4 + log(1.1)*D5 +
            log(beta_IC)*IC + log(beta_ID)*ID + log(beta_U)*U;
        prob_subj = 1/(1+exp(-lin_comb));
        Y = rand("Bernoulli",prob_subj);
        output;
    end;
    drop rand_sd sd rand_p0 rand_RD p1 alpha rand_beta_IC beta_IC rand_beta_ID beta_ID rand_beta_U beta_U lin_comb prob_subj;
run;

proc freq data=simul1;
    tables center Y;
run;

proc means data=simul1 n mean std min max;
    var C1 C2 C3 C4 C5;
run;
