import numpy as np


def sgd(X, y, w, b, eta):


    '''
    w: the coefficients
    b: the intercept
    eta: learning rate

    '''
    

    '''calculate the predicted y for linear regression'''
    yhat = b + X @ w
    
    '''
    Update
    The calculation process has been documented in homework-4.rmd
    '''

    w = w - eta * 2 * X * (yhat - y)
    b = b - eta * 2 * (yhat - y)
    
    return w, b


    
