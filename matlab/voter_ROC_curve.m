function [roc,roc_curve,surfer] = voter_ROC_curve_b(org_mat,kernel,folds,liner)

%
% [roc,roc_curve,surfer]=voter_ROC_curve_b(org_mat,kernel,folds,liner)
% liner of form linspace(0,1,n)

% Set up training/testing data
org_mat = logical(org_mat);
[a,b] = find(org_mat);
for i = 1:folds
	test_org_mat{i}=org_mat-sparse(a(i:(folds):length(a)),b(i:(folds):length(a)),1,size(org_mat,1),size(org_mat,2));
end
test_org_mat = cell2mat(test_org_mat);


% Calculate the predicted ranks
sumin =(kernel*test_org_mat);
sumall =(repmat(sum(kernel),size(test_org_mat,2),1))';
predicts = sumin./sumall;
predicts(logical(test_org_mat))=nan;
predicts = tiedrank(full(predicts));
predicts0 = predicts;


% Generate ROC curve
temp = repmat(org_mat,1,folds)&~isnan(predicts0);
for m = 1:size(predicts0,2)
	[~,j2] = sort(predicts0(:,m));
	temp(:,m)=temp(j2,m);
end

predicts0 = isnan(predicts0);

for m=1:size(predicts0,2)
	predicts0(:,m)=predicts0(j2,m);
end

predicts0 = logical((sum(reshape(full(predicts0),size(predicts0,1),[],folds),3)));
temp = (sum(reshape(full(temp),size(temp,1),[],folds),3));

for m=1:size(predicts0,2)
	sub = temp(:,m);
	sub(predicts0(:,m)) =[];
	roc_curve{m} = [1-cumsum (~sub)/sum(~sub),1-cumsum(sub)/sum(sub)];
end

surfer=zeros(length(liner),length(liner));

for m=1:size(predicts0,2)
    [~,X2,~] = unique(roc_curve{m}(:,1));
    Y2 = interp1(roc_curve{m}(X2,1),roc_curve{m}(X2,2),liner);
    Y2(end) = 1;
    newY = floor(Y2/liner(2))+1;
    for m2 = 1:length(liner)
        surfer(m2,newY(m2)) = surfer(m2,newY(m2))+1;
    end
end
surfer=surfer';

%done

% Calculate the ROC analytically
filter = repmat(org_mat,1,folds);
predicts(~filter)=0;
np = sum(filter)-sum(test_org_mat);
nn = size(test_org_mat,1)-sum(filter);
roc = (nansum(predicts)./np-(np+1)/2)./nn;
roc = reshape(roc,size(org_mat,2),folds);
roc = nanmean(roc,2);

