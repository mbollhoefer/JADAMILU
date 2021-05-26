function [V,D,options] = PJD(A,M,k,sigma,options,PREC)
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


% flag for generalized eigenvalue problem
GEP=0;

% flag for external preconditioning
EXPREC=0;

% strategy where to seek for eigenvalues
strategy='l';

n=size(A,1);
if norm(A-A',inf)~=0 || size(A,2)~=n
   disp('A must be a square symmetrix(Hermitian) matrix');
   return
end

% PJD(A)
if nargin==1
   k=min(n,6);
   M=[];
end

if nargin>=2
   Min=M;
end
if nargin>=3
   kin=k;
else
   k=6;   
end
if nargin>=4
   sigmain=sigma;
else
   sigma='l';   
end
% default options
myoptions=PJDinit(A);
if nargin>=5
   optionsin=options;
else
   options=myoptions;
end


% PJD(A,M) or PJD(A,k) or PJD(A,[])
% check second argument
if nargin>=2
   % M might be provided (or k)
   if ~isempty(M)
      % trivial
      if n==1 
	 if size(M,1)==n && size(M,2)==n
	    V=1; D=A/M;
	    if nargout==3
	       options.niter=0;
	       options.res=0;
	       options.info=0;
	    end
	    return
	 else
	    disp('M must be a square symmetrix(Hermitian) matrix of same size as A')
	    return
	 end
      elseif size(M,1)==n && size(M,2)==n
	 GEP=1;
	 k=6;
	 if norm(M-M',inf)~=0
	    disp('M must be a square symmetrix(Hermitian) matrix of same size as A')
	    return
	 end
      elseif size(M,1)==1 && size(M,2)==1
	 k=M; M=[];
	 if k~=round(k) || k<=0 || k>n
	    disp('k must be a positive integer between 1 and n')
	    return
	 end
      else
         disp('M must be a square symmetrix(Hermitian) matrix of same size as A')
	 return
      end
   else
      k=6;
   end
end

% PJD(A,M,k) or PJD(A,k,sigma) or PJD(A,[],k)
% check third argument
if nargin>=3
   % PJD(A,M,k,...)
   if ~isempty(M)
      k=kin;
      if k~=round(k) || k<=0 || k>n
	 disp('k must be a positive integer between 1 and n')
	 return
      end
   % PJD(A,k,sigma,...) or PJD(A,[],k,...)
   else
      if ~isempty(Min) 
	 k=Min;
	 sigma=kin;
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
	    if (~isnumeric(strategy) || size(strategy,1)~=1 || ...
		size(strategy,2)~=1  || ~isreal(strategy))
	       disp('sigma must be a real scalar')
	       return
	    end
	 end
      else % isempty(Min) => PJD(A,[],k)
	 k=kin;
	 if k~=round(k) || k<=0 || k>n
	    disp('k must be a positive integer between 1 and n')
	    return
	 end
      end
   end
end


% PJD(A,M,k,sigma) or PJD(A,k,sigma,options) or PJD(A,[],k,sigma)
% check fourth argument
if nargin>=4
   % PJD(A,M,k,sigma)
   if ~isempty(M)
      sigma=sigmain;
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
	 if (~isnumeric(strategy) || size(strategy,1)~=1 || ...
	     size(strategy,2)~=1  || ~isreal(strategy))
	    disp('sigma must be a real scalar')
	    return
	 end
      end
   % PJD(A,k,sigma,options) or PJD(A,[],k,sigma)
   else
      % PJD(A,k,sigma,options)
      if ~isempty(Min)
	 options=sigmain;
	 if ~isfield(options, 'madspace')
	    options.madspace=myoptions.madspace;
	 end   
	 if ~isfield(options, 'maxit')
	    options.maxit=myoptions.maxit;
	 end   
	 if ~isfield(options, 'restol')
	    options.restol=myoptions.restol;
	 end   
	 if ~isfield(options, 'disp')
	    options.disp=myoptions.disp;
	 end   
	 if ~isfield(options, 'V0')
	    options.V0=myoptions.V0;
	 end   
	 if ~isfield(options, 'mem')
	    options.mem=myoptions.mem;
	 end   
	 if ~isfield(options, 'droptol')
	    options.droptol=myoptions.droptol;
	 end   
	 if ~isfield(options, 'condest')
	    options.condest=myoptions.condest;
	 end   
	 
      % PJD(A,[],k,sigma)
      else
	 sigma=sigmain;
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
	    if (~isnumeric(strategy) || size(strategy,1)~=1 || ...
	        size(strategy,2)~=1  || ~isreal(strategy))
	       disp('sigma must be a real scalar')
	       return
	    end
	 end
      end
   end
end


% PJD(A,M,k,sigma,options) or PJD(A,k,sigma,options,PREC) or PJD(A,[],k,sigma,options)
% check fifth argument
if nargin>=5
   % PJD(A,M,k,sigma,options)
   if ~isempty(M)
      options=optionsin;
      if ~isfield(options, 'madspace')
	 options.madspace=myoptions.madspace;
      end   
      if ~isfield(options, 'maxit')
	 options.maxit=myoptions.maxit;
      end   
      if ~isfield(options, 'restol')
	 options.restol=myoptions.restol;
      end   
      if ~isfield(options, 'disp')
	 options.disp=myoptions.disp;
      end   
      if ~isfield(options, 'V0')
	 options.V0=myoptions.V0;
      end   
      if ~isfield(options, 'mem')
	 options.mem=myoptions.mem;
      end   
      if ~isfield(options, 'droptol')
	 options.droptol=myoptions.droptol;
      end   
      if ~isfield(options, 'condest')
	 options.condest=myoptions.condest;
      end   

   % PJD(A,k,sigma,options,PREC) or PJD(A,[],k,sigma,options)
   else
      % PJD(A,k,sigma,options,PREC)
      if ~isempty(Min)
         PREC=optionsin;
	 if ~isa(PREC,'function_handle') && ~isstr(PREC)
	    disp('PREC must be provided as a function handle or string')
	    return
	 end
	 EXPREC=1; 
	 if isa(PREC,'function_handle')
	    PRECNAME=func2str(PREC);
	 else
	    PRECNAME=PREC;
	 end

      % PJD(A,[],k,sigma,options)
      else
	 options=optionsin;
	 if ~isfield(options, 'madspace')
	    options.madspace=myoptions.madspace;
	 end   
	 if ~isfield(options, 'maxit')
	    options.maxit=myoptions.maxit;
	 end   
	 if ~isfield(options, 'restol')
	    options.restol=myoptions.restol;
	 end   
	 if ~isfield(options, 'disp')
	    options.disp=myoptions.disp;
	 end   
	 if ~isfield(options, 'V0')
	    options.V0=myoptions.V0;
	 end   
	 if ~isfield(options, 'mem')
	    options.mem=myoptions.mem;
	 end   
	 if ~isfield(options, 'droptol')
	    options.droptol=myoptions.droptol;
	 end   
	 if ~isfield(options, 'condest')
	    options.condest=myoptions.condest;
	 end   
      end
   end
end


% PJD(A,M,k,sigma,options,PREC)
if nargin==6
   if ~isa(PREC,'function_handle') && ~isstr(PREC)
      disp('PREC must be provided as a function handle or string')
      return
   end
   EXPREC=1; 
   if isa(PREC,'function_handle')
      PRECNAME=func2str(PREC);
   else
      PRECNAME=PREC;
   end
end



% M,GEP,k,strategy, options, EXPREC
if ~GEP
   if ~EXPREC
      if isreal(A)
         [V,D,options]=DSYMjadamilu(A,k,sigma,options);
      else
         [V,D,options]=ZHERjadamilu(A,k,sigma,options);
      end
   else
      if isreal(A)
         [V,D,options]=DSYMjadamilurevcom(A,k,sigma,options,PRECNAME);
      else
         [V,D,options]=ZHERjadamilurevcom(A,k,sigma,options,PRECNAME);
      end
   end
else
   if ~EXPREC
      if isreal(A)
         [V,D,options]=DSYMjadamilu_gep(A,M,k,sigma,options);
      else
         [V,D,options]=ZHERjadamilu_gep(A,M,k,sigma,options);
      end
   else
      if isreal(A)
         [V,D,options]=DSYMjadamilurevcom_gep(A,M,k,sigma,options,PRECNAME);
      else
         [V,D,options]=ZHERjadamilurevcom_gep(A,M,k,sigma,options,PRECNAME);
      end
   end
end


