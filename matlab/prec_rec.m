function [MAP,prec,tpr] = prec_rec(scores,org_mat)

%
% [MAP,prec,rec] = prec_rec2(scores,org_mat)
% index 1s and 0s, score increasing
%
scores=scores(:);

for r = 1:size(org_mat,2)
	index = org_mat(:,r);
	index = logical(index);
	j = tiedrank(-scores,'descend');
	j = j(index);
	i = tiedrank(-scores(index));
	MAP(r) = nanmean(i./j);
	prec = (i./j);
	tpr = (i)/(sum(index));
	[m,n] = sort(tpr);
	tpr = tpr(n);
	prec = prec(n);
end
