from change_detector import ChangeDetector
from scaffold import OnlineSimulator

import pandas
import numpy as np
import statsmodels.tsa.api as tsa


class CusumDetector(ChangeDetector):
    """
    CUSUM Change Detector
    
    Residuals:
    
    Stopping Rule:
    """

    def __init__(self):
        super(CusumDetector, self).__init__()

    def update_residuals(self, new_signal_value):
        raise NotImplementedError

    def check_stopping_rules(self, new_signal_value):
        raise NotImplementedError


def main(noise=0.0):
    # make a signal with one change
    signal = np.ones(150)
    signal[:100] *= 50
    signal[100:] *= 40

    # if we want a noisy signal...
    if noise > 0.0:
        # figure out the size of the change and make noise of that size
        jump_size = signal[0] - signal[-1]
        noise = np.random.normal(
            size=signal.shape,
            scale=jump_size * noise)
        signal += noise

    detector = CusumDetector()
    OnlineSimulator(detector, signal).run(plot=True)


if __name__ == '__main__':
    main()
