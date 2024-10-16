function [V,D,options] = PJD(A,sizea,M,k,sigma,options,PREC)
% [V,D,options] = PJD(A,M,k,sigma,options,PREC)
% 
% find eigenvalues and eigenvalues of a symmetric(Hermitian) matrix A using
% preconditioned Jacobi-Davidson as implemented in JADAMILU
%
%
% [V,D] = PJD(A) returns a diagonal matrix D of A's 6 largest eigenvalues and a
% matrix V whose columns are the corresponding eigenvectors using the default
% options 
% 
% [V,D,options] = PJD(A) also return the options used as well as the number of
% matrix-vector multiplications, the success of the method and the residuals
%
% PJD(A,M) solves the generalized eigenvalue problem A*V == M*V*D. M must be
% the same size as A and symmetric(Hermitian) positive definite. PJD(A,[],...)
% indicates the standard eigenvalue problem A*V == V*D.
% 
% PJD(A,k) and PJD(A,M,k) return the k largest eigenvalues.
% 
% PJD(A,k,sigma) and PJD(A,M,k,sigma) return k eigenvalues.
% If sigma is: 'l'         largest eigenvalues
%              's'         smallest eigenvalues
% If sigma is a real scalar including 0, PJD finds the eigenvalues closest to
% sigma. 
%
% PJD(A,k,sigma,options) and PJD(A,M,k,sigma,options) specify options as
% provided by PJDinit.
%
% PJD(A,k,sigma,options,PREC) and PJD(A,M,k,sigma,options,PREC) use a function
% handle PREC as preconditioner rather than JADAMILU's built-in multilevel ILU
%
%
% if 'A' is a string or a function handle, then 'A' must be followed by the
% dimension 'sizea' of 'A', e.g. [V,D]=PJD(A,sizea,k,sigma,options,PREC)
% Note that PJD only accepts a string or a function handle if you provide
% your own preconditioner 'PREC'



  
optionsin=PJDinit(A);
A_matvec=(isa(A,'function_handle') || isstr(A));
M_matvec=(nargin>=3 && (isa(M,'function_handle') || isstr(M)));


% check trivial 1x1 eigenvalue problem for consistency
if A_matvec
   if nargin<2
      fprintf('if a string or function handle is passed as A, then you must provide as second parameter its scalar dimension\n');
      return
   end % if
   n=sizea;
  
   % PJD(A,sizea,M,k,   sigma,options,PREC)
   % or
   % PJD(A,sizea,k,     sigma,options,PREC)

   if n==1 && ~M_matvec
      % k or M? Both have to be 1x1
      if nargin>=3
	 if size(M,1)~=1 || size(M,2)~=1
	    fprintf('third input must be a scalar\n');
	    return
	 end
      end % if

      options=optionsin;
      options.niter=0;
      options.info=0;
      options.res=0;
      V=1;
      if isa(A,'function_handle')
	 DA=A(1);
      else
	 DA=feval(A,1);
      end % if-else
      % PJD(A,sizea)
      if nargin==2
	 return
      % PJD(A,sizea,M,...) or PJD(A,sizea,k,...)
      % if k is provided, only k==1 makes sense
      elseif nargin>=3
	 if M==1
	    return
	 else
            DA=DA/M;
	    return
	 end
       end % if
   elseif n==1
       % M provided as function handle or string
       if isa(M,'function_handle')
	  DM=M(1);
       else
          DM=feval(M,1);
       end
	
       options=optionsin;
       options.niter=0;
       options.info=0;
       options.res=0;
       V=1;
       if isa(A,'function_handle')
	  DA=A(1);
       else
	  DA=feval(A,1);
       end % if-else
       % PJD(A,sizea,M,...) or PJD(A,sizea,M,k,...)
       % if k is provided, only k==1 makes sense
       D=DA/DM;
       return
   end % if-elseif n=1 and not M_matvec
   
else
   % PJD(A,M,k,   sigma,options,PREC)
   % or
   % PJD(A,k,     sigma,options,PREC)
   n=size(A,1);
   if n==1
      % k or M? Both have to be 1x1
      if nargin>=2
	 if M_matvec
	    fprintf('second input be explicitly given as matrix\n');
	    return
	 end
	 if size(M,1)~=1 || size(M,2)~=1
	    fprintf('second input must be a scalar\n');
	    return
	 end
      end % if

      options=optionsin;
      options.niter=0;
      options.info=0;
      options.res=0;
      V=1;
      DA=A;
      % PJD(A)
      if nargin==1
	 return
      % PJD(A,M,...) or PJD(A,k,...)
      % if k is provided, only k==1 makes sense
      elseif nargin>=2
	 if M==1
      	    return
	 else
            D=DA/M;
	    return
	 end
      end % if
   end % if
end
% END trivial 1x1 case




% flag for generalized eigenvalue problem
GEP=1;

% flag for external preconditioning
EXPREC=0;

flag_matvec=0;
sigmain='l';
if A_matvec
   if nargin<2
      fprintf('if a string or function handle is passed as A, then you must provide as second parameter its scalar dimension\n');
      return
   end % if

   if isa(A,'function_handle')
      ANAME=func2str(A);
   else
      ANAME=A;
   end % if-else
   n=sizea;
   kin=min(n,6);
   
   if size(sizea,1)~=1 || size(sizea,2)~=1
      fprintf('if a string or function handle is passed as A, then the next parameter must be its scalar dimension\n');
      return;
   end % if
   
   % check whether M is passed or not
   % no, and no further input
   if nargin<3
      GEP=0;
      k=kin;
      sigma=sigmain;   
      options=optionsin;
   else % nargin>=3
      if M_matvec
	 if isa(M,'function_handle')
	    MNAME=func2str(M);
	 else
	    MNAME=M;
	 end % if-else
	 if nargin==7
	    EXPREC=1;
	 end 
      elseif size(M,1)~=n || size(M,2)~=n
         % no, but something else, thus shift!
	 GEP=0;
	 % shift input variables
	 if nargin>=6
	    PREC=options;
	    EXPREC=1;
	 end 
	 if nargin>=5
	    options=sigma;
	 else
	    options=optionsin;
	 end 
	 if nargin>=4
	    sigma=k;
	 else
	    sigma=sigmain;   
	 end 
	 k=M;
      else % M explictly given, function name for A
	 if nargin==7
	    EXPREC=1;
	 end 
      end % if-elseif
   end % if-else
else % matrix explicitly given, no sizea

   n=size(A,1);
   kin=min(n,6);
   
   if ~ishermitian(A)
      disp('A must be a square symmetrix(Hermitian) matrix');
      return
   end
   
   % check whether M is passed or not
   % no, and no further input
   if nargin<2
      GEP=0;
      k=kin;
      sigma=sigmain;   
      options=optionsin;
   else % nargin>=2
      % no, but something else, thus shift!
      if isa(sizea,'function_handle') || isstr(sizea)
	 fprintf('if A is explicitly passed, so must M\n');
	 return;
      end
      if size(sizea,1)~=n || size(sizea,2)~=n
	 GEP=0;
	 % shift input variables without M
         if nargin>=5
            PREC=sigma;  
	    EXPREC=1;
         end 
         if nargin>=4
            options=k;
	 else
	    options=optionsin;
         end 
         if nargin>=3
            sigma=M;  
	 else
	    sigma=sigmain;   
         end 
         k=sizea;
      else % yes
         % shift input variables respecting M
         if nargin>=6
            PREC=options;  
	    EXPREC=1;
         end 
         if nargin>=5
            options=sigma;
	 else
	    options=optionsin;
	 end 
	 if nargin>=4
	    sigma=k;  
	 else
	    sigma=sigmain;   
	 end 
	 if nargin>=3
	    k=M;
	 else
	    k=kin;
	 end
	 M=sizea;
      end % if
   end % if-else
end % if





% check M
if GEP
   if ~M_matvec && ~ishermitian(M)
      disp('M must be a square symmetrix(Hermitian) matrix of same size as A')
      return
   end
end


% check k
if k~=round(k) || k<=0 || k>n
   disp('k must be a positive integer between 1 and n')
   return
end

% check sigma
if isstr(sigma)
   if sigma=='l' || sigma=='L'
      strategy='l';
   elseif sigma=='s' || sigma=='S'
      strategy='s';
   else
      disp('string sigma must be either `l´ or `s´')
      return
   end
else
   strategy=sigma;
   if ~isnumeric(strategy) || size(strategy,1)~=1 || ...
      size(strategy,2)~=1  || ~isreal(strategy)
      disp('sigma must be a real scalar')
      return
   end
end

% check options
if ~isfield(options, 'madspace')
    options.madspace=optionsin.madspace;
end   
if ~isfield(options, 'maxit')
   options.maxit=optionsin.maxit;
end   
if ~isfield(options, 'restol')
   options.restol=optionsin.restol;
end   
if ~isfield(options, 'disp')
   options.disp=optionsin.disp;
end   
if ~isfield(options, 'V0')
   options.V0=optionsin.V0;
end   
if ~isfield(options, 'mem')
   options.mem=optionsin.mem;
end   
if ~isfield(options, 'droptol')
   options.droptol=optionsin.droptol;
end   
if ~isfield(options, 'condest')
   options.condest=optionsin.condest;
end   

% check PREC (as far as it exists)
if EXPREC
   if isa(PREC,'function_handle')
      PRECNAME=func2str(PREC);
   elseif isstr(PREC)
      PRECNAME=PREC;
   else
      disp('PREC must be provided as a function handle or string')
      return
   end
end % if

if A_matvec && ~EXPREC
   disp('A must be explicitly given if no preconditoner is available');
   return
end % if


% matrix A explictly stated
if ~A_matvec
   Adgl=sign(diag(A));
   I=find(Adgl==0);
   Adgl(I)=1;
   ADgl=spdiags(realmin*Adgl,0,n,n);
   % M,GEP,k,strategy, options, EXPREC
   if ~GEP
      if ~EXPREC
	 if isreal(A)
            [V,D,options]=DSYMjadamilu(A+ADgl,k,sigma,options);
	 else
            [V,D,options]=ZHERjadamilu(A+ADgl,k,sigma,options);
	 end % if-else
      else
	 if isreal(A)
            [V,D,options]=DSYMjadamilurevcom(A+ADgl,k,sigma,options,PRECNAME);
	 else
            [V,D,options]=ZHERjadamilurevcom(A+ADgl,k,sigma,options,PRECNAME);
	 end % if-else
      end % if-else ~EXPREC
   else % GEP
      Mdgl=sign(diag(M));
      I=find(Mdgl==0);
      Mdgl(I)=1;
      MDgl=spdiags(realmin*Mdgl,0,n,n);
      if ~EXPREC
	 if isreal(A)
            [V,D,options]=DSYMjadamilu_gep(A+ADgl,M+MDgl,k,sigma,options);
	 else
            [V,D,options]=ZHERjadamilu_gep(A+ADgl,M+MDgl,k,sigma,options);
	 end % if-else
      else
	 if isreal(A)
            [V,D,options]=DSYMjadamilurevcom_gep(A+ADgl,M+MDgl,k,sigma,options,PRECNAME);
	 else
            [V,D,options]=ZHERjadamilurevcom_gep(A+ADgl,M+MDgl,k,sigma,options,PRECNAME);
	 end % if-else
      end % if-else ~EXPREC
   end % if-else GEP
else % only mat-vec passed, then a preconditioner is mandatory
   % M(resp. MNAME),GEP,k,strategy, options, EXPREC
   if ~GEP
       if isfield(options,'isreal') && options.isreal
          [V,D,options]=DSYMjadamilurevcom_matvec(ANAME,n,k,sigma,options,PRECNAME);
       else
          [V,D,options]=ZHERjadamilurevcom_matvec(ANAME,n,k,sigma,options,PRECNAME);
       end
   else % GEP
      if ~M_matvec
	 Mdgl=sign(diag(M));
	 I=find(Mdgl==0);
	 Mdgl(I)=1;
	 MDgl=spdiags(realmin*Mdgl,0,n,n);
	 if isfield(options,'isreal') && options.isreal
	    [V,D,options]=DSYMjadamilurevcom_gep_matvec(ANAME,n,M+MDgl,k,sigma,options,PRECNAME);
	 else
	    [V,D,options]=ZHERjadamilurevcom_gep_matvec(ANAME,n,M+MDgl,k,sigma,options,PRECNAME);
	 end % if-else
      else % M_matvec
	 if isfield(options,'isreal') && options.isreal
	    [V,D,options]=DSYMjadamilurevcom_gep_matvec_matvec(ANAME,n,MNAME,k,sigma,options,PRECNAME);
	 else
	    [V,D,options]=ZHERjadamilurevcom_gep_matvec_matvec(ANAME,n,MNAME,k,sigma,options,PRECNAME);
	 end % if-else
      end % if-else ~M_matvec
   end % if-else ~GEP
end % if-else A_matvec
