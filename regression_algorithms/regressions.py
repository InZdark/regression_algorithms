import numpy as np


class OLS():
    # Only for y = w1*x1 + w0
    def fit(self, X, y):
        shape = X.shape
        N = shape[0]
        d = np.mean(X**2) - np.mean(X)**2
        self.w_0 = (np.mean(y) * np.mean(X**2) -\
                    np.mean(X) * np.mean(X * y))/d
        self.w_1 = (np.mean(X * y) - np.mean(X) * np.mean(y))/d
    def predict(self, X):
        y_hat = self.w_1*X + self.w_0
        return y_hat


class Regressor():
    def fit(self, X, y):
        self.X = X
        self.y = y

    def predict(self, X, k, epsilon = 1e-3):
        N = len(X)
        y_hat = np.zeros(N)
        for i in range(N):
            dist = np.sum((self.X -X[i])**2, axis = 1)
            idx = np.argsort(dist)[:k]
            gamma_k = 1/(np.sqrt(dist[idx]) + epsilon)
            y_hat[i] = gamma_k.dot(self.y[idx])/gamma_k.sum()
        return y_hat

class KNNRegressor():
    def fit(self, X, y):
        self.X = X
        self.y = y

    def predict(self, X, k):
        N = len(X)
        y_hat = np.zeros(N)
        for i in range(N):
            dist = np.sum((self.X -X[i])**2, axis = 1)
            idx = np.argsort(dist)[:k]
            gamma_k = np.exp(-dist[idx])/np.exp(-dist[idx]).sum()
            y_hat[i] = gamma_k.dot(self.y[idx])
        return y_hat

class MultiLinearRegressor():
    def fit(self, X, y):
        self.weights = np.linalg.solve(X.T@X, X.T@y)
    def predict(self, X):
        return np.matmul(X, self.weights)
