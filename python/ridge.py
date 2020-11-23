import numpy as np

def ridge_reg(X, y, lambda_ridge):

  '''
  rewrite my R function in python
  Sigma = diag(svd_x$d)
  lambda_I = diag(rep(lambda, length(svd_x$d)))
  beta = V * (Sigma + lambda I) ^{-1} Sigma U^T Y
  '''
  
  U, S, Vt = np.linalg.svd(X, full_matrices = False)
  D = np.diag(S / (S ** 2 + lambda_ridge))
  
  return Vt.T.dot(D).dot(U.T).dot(y)
