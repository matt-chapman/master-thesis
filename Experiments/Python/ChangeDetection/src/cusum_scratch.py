import numpy as np
import statsmodels.tsa.api as tsa
import pandas as pd


def main():

    signal = np.ones(150)
    signal[:100] *= 50
    signal[100:] *= 40

    dates = pd.date_range('1/1/2017', periods=150, freq='D')

    ts = pd.Series(signal, index=dates)

    df = pd.DataFrame(ts)

    data = df.as_matrix()

    model = tsa.VAR(data)
    results = model.fit()
    residuals = [np.matrix(e).T for e in list(results.resid)]

if __name__ == '__main__':
    main()
