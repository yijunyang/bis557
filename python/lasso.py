import numpy as np

'''
refer to CASL chap7 casl_lenet function
'''

def soft_thresh(a, b):

    '''
    define the soft threshold function
    refer to CASL page 189

    a: numeric vector of the soft-thresholded values of a
    b: the soft thresholded value
    '''
    
    if np.abs(a) <= b:
        a = 0
    elif a > 0:
        a = a - b
    elif a < 0:
        a = a + b
    return a
  

def update_beta(X, y, lambda_lasso, b, W):
    '''
    update beta vector using coordinate descent
    refer to CASL page 190
    
    X: a numeric data matrix
    y: response vector
    lambda_lasso: the penalty term
    b: a vector of warm start coefficients for the algorithm
    W: a vector of sample weights
    '''

    WX = W * X
    WX2 = W * np.power(X, 2)
    Xb = X @ b

    for i in range(int(len(b))):
        
        Xb = Xb - X[:,i].reshape(-1,1) * b[i]
        b[i] = soft_thresh(np.sum(WX[:,i] * (y - Xb).reshape(-1)), lambda_lasso)
        b[i] = b[i] / np.sum(WX2[ :,i])
        Xb = Xb + np.outer(X[:,i], b[i])
    return b

def coord_descent(X, y, lambda_lasso, tol = 1e-4, maxit = 1e3):

    '''
    compute linear elastic net using coordinate descent
    refer to CASL page 191
    
    X: a numeric data matrix
    y: response vector
    lambda_lasso: the penalty term
    maxit: integer maximum number of iterations
    tol: numeric tolerance parameter
    '''
    
    b = np.ones((X.shape[1], 1))
    W = np.array(np.repeat(1, len(y))/len(y))[:, np.newaxis]
    
    for i in range(int(maxit)):
        b_old = b.copy()
        b = update_beta(X, y, lambda_lasso, b, W)
        
        if np.all(np.abs(b) - np.abs(b_old) < tol):
            break

        if i == maxit - 1:
            print("Function did not converge.")

    return b
