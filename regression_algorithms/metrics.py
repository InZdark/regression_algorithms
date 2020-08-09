import numpy as np

def r_square(y, y_hat):
    sst = np.mean((y - np.mean(y))**2)
    sse = np.mean((y_hat - y)**2)
    return 1 - (sse/sst)

def mean_square_error(y, y_hat):
    return np.mean((y - y_hat)**2)

# def regressionAcc(y, y_hat):
#     error = y - y_hat
#     return np.mean(1 - np.abs(error/y))
