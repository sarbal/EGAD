function [roc] = voter_ROC(org_mat,kernel,folds)

%
%  function  [roc]  = voter_ROC(org_mat,kernel,folds)
%
%      Calculates the AUROC using n-fold cross validation for
%	a given feature labels (org_mat) and network (kernel)
%

% Set up training/testing data
org_mat = logical(org_mat);
[a,b] = find(org_mat);
for i = 1:folds
	test_org_mat{i} = org_mat-sparse(a(i:(folds):length(a)),b(i:(folds):length(a)),1,size(org_mat,1),size(org_mat,2));
end

test_org_mat = cell2mat(test_org_mat);

% Calculate the predicted ranks
sumin = (kernel*test_org_mat);
sumall = (repmat(sum(kernel),size(test_org_mat,2),1))';
predicts = sumin./sumall;
predicts(logical(test_org_mat))=nan;
predicts = tiedrank(full(predicts));

% Calculate the ROC analytically
filter = repmat(org_mat, 1, folds);
predicts(~filter) = 0;
np = sum(filter) - sum(test_org_mat);
nn = size(test_org_mat,1) - sum(filter);
roc = (nansum(predicts)./np-(np+1)/2)./nn;
roc = reshape(roc,size(org_mat,2),folds);
roc = nanmean(roc,2);