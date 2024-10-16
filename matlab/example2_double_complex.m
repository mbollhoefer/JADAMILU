n=1000;
maxeig=5;
i=sqrt(-1);	
A=spdiags([-5*i*ones(n,1) (0:n-1)' 5*i*ones(n,1)],-1:1,n,n);
M=spdiags([-.25*i*ones(n,1) ones(n,1) .25*i*ones(n,1)],-1:1,n,n);

PJDoptions=PJDinit(A)

tic
[V,Lambda,PJDoptions]=PJD(A,M,maxeig,'s',PJDoptions);
toc


format long e

fprintf('       Eigenvalue            Res. norm\n');
full([diag(Lambda), PJDoptions.res])

fprintf('Number of MATVEC needed: %5d ;  Estimated gap: %12.4e ;  INFO: %d\n',...
	PJDoptions.niter, PJDoptions.gap, PJDoptions.info);

fprintf('Suggested DROPTOL if restart needed: %8.1e\n', PJDoptions.droptol);


% similar command using MATLAB eigs function
% [Veigs, Lambdaeigs]=eigs(A,M,maxeig,'sr');[~,I]=sort(diag(Lambdaeigs)); Veigs=Veigs(:,I); Lambdaeigs=Lambdaeigs(I,I); diag(Lambdaeigs)
