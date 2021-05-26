function options = PJDinit(A)
% options = PJDinit(A)
% 
% init structure of options to their default values for a given nxn matrix A
% 
% input
% -----
% A         nxn matrix
%
% output
% ------
% options   structure with default parameters
%
% ------------------------------------------------------------------------
%
% possible options:
%  1. options.madspace
% --------------------
%    Maximal dimension of the search space (usually between 10 and 20). Should
%    be at least 2.
%    default value: 20
%
%
%  2. options.maxit
% --------------------
%    maximum number of matrix vector multiplications; should be positive
%    default value: 5000
%
%
%  3. options.niter
% --------------------
%    number of matrix vector multiplications needed by JADAMILU
%    default value: empty
%
%
%  4. options.restol
% --------------------
%    The tolerance on residual norm. Iterations to compute eigenvector number i
%    are stopped whenever ||A x_i − lambda_i x_i||_2 <= restol
%    default: 1e-10
%    
%
%  5. options.res
% --------------------
%    Residual norms: res(i) = ||A x_i − lambda_i x_i||_2
%    default: empty
%    
%
%  6. options.gap
% --------------------
%    The estimated distance between the set of computed eigenvalues and the
%    remaining part of the spectrum; may be inaccurate.
%    default: empty
%    
%
%  7. options.info
% --------------------
%    "info"= 0  if normal termination.
%    "info"> 0  if allowed maximum number of matrix vector multiplications
%               performed without finding all wanted eigenvalues & eigenvectors.
%    "info"< 0  if an error occurred - see printed output for details
%    default: empty
% 
%
%  8. options.V0
% --------------------
%    initial guess "V0" provided for the eigenvectors sought
%    default: empty
%
%  9. options.disp
% --------------------
%    If zero, only error messages are printed on standard output. Otherwise,
%    its sign indicates the level of output: if negative, extensive information
%    (for expert) is provided; most users should be satisfied with the
%    information provided for positive "disp"
%    default: 0
%   
%
%
% 10. options.mem
% -------------------
%    only required when using JADAMILU's built-in preconditioner
%
%    "mem" prescribes the amount of memory the user is willing to spend for the
%    preconditioner. "mem" is relative to the number of nonzero of the input
%    matrix. If it turns out the preconditioner that is computed does not fit
%    into the memory area that is offered by the user, JADAMILU will terminate
%    with an error message. In this case one can either increase "mem" (if
%    there is more memory available) or one has to increase "droptol" threshold
%    for dropping small entries during the computation of the incomplete LU
%    decomposition 
%    default: 10.0
%
%
% 11. options.droptol
% --------------------
%    only required when using JADAMILU's built-in preconditioner
%
%    drop tolerance for the multilevel incomplete factorization. A small drop
%    tolerance will typically lead to more fill-in, i.e. more memory will be
%    consumed and the application of the preconditioner is more costly. On the
%    other hand, the number of iteration steps is expected to be less for a
%    smaller drop tolerance. 
%    default: 1e-2
%
%
% 12. options.condest
% -------------------
%    only required when using JADAMILU's built-in preconditioner
% 
%    bound for the inverse triangular factors from the incomplete LU 
%    decomposition
%    default: 5
%
%
% 13. options.shift
% -------------------
%    if we are to compute the smallest or largest eigenvalues and we want to
%    use a different shift for preconditioning rather than sigma we could
%    provide a different real shift for preconditioning here  
%    default: not set
%
%
% 14. options.sigma0
% -------------------
%    if we are to compute the smallest or largest eigenvalues and we want to
%    provide an initial guess for the smallest/largest eigenvalue we may use
%    sigma0 for this purpose
%    default: not set
%
%
% 15. options.noadaptprec
% -----------------------
%    set this to a nonzero value to turn off adaptive preconditioning inside
%    JADAMILU. In this case JADAMILU will compute its internal preconditioner
%    only once and keep it throughout the eigenvector computation
%    default: not set


options.madspace=20;
options.maxit=5000;
options.niter=[];
options.restol=1e-10;
options.res=[];
options.gap=[];
options.info=[];
options.V0=[];
options.disp=0;
options.mem=10;
options.droptol=1e-2;
options.condest=5;
