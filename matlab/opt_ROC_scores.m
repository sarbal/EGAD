function opt_scores=opt_ROC_scores(org_mat);

%
% opt_scores = opt_ROC_scores(org_mat)
%

vec_mult = sum(org_mat).*(size(org_mat,1)-sum(org_mat));
opt_scores = (1./vec_mult)*org_mat';

end