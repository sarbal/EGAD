function [roc,scores1] = voter(index,kernel,folds)

%
% [roc] = voter(index,kernel,folds)
%
%

rand('twister',sum(100*clock));

scores1 = sparse(length(index),1);
scores2 = sparse(length(index),1);

index = (real(index));
whichers = find(index);

sizer = length(whichers);
whichers = whichers(randperm(sizer));
output = [];
output2 = [];
for i = 1:folds
    whicherout{i} = whichers(floor((i-1)/folds*(sizer)+1):floor(sizer*i/folds));
end

p2 = zeros(length(index),1);
scores2 = p2;

for i=1:folds
	newindex = index;
	newindex(whicherout{i})=0;
	newindex2 = newindex;
	newindex2 = logical(newindex2);
%    [p, k, wts] = predictClassesCG(newindex2,kernel);
	p = (sum(kernel(newindex2,:))./sum(kernel(~newindex2,:)))';
	p(logical(newindex)) = nan;
	p = tiedrank(p);
	scores2(whicherout{i}) = p(whicherout{i});
	output = [output;p(whicherout{i})];
	p2 = p2+p;
end

scores1 = p/folds;
scores1(isnan(scores1))=scores2(isnan(scores1));
scores1 = tiedrank(scores1);

roc = sum(output(:))/folds;
np = sum(index)/folds;
nn = length(index)-sum(index);
roc = (roc/np-(np+1)/2)/nn;