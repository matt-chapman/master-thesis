from change_detector import ChangeDetector
from scaffold import OnlineSimulator, plot_signal_and_residuals

import numpy as np
import matplotlib.pyplot as plt

signal = np.ones(150)
signal[:100] *= 50
signal[100:] *= 40


class MeanDetector(ChangeDetector):
    """
    Static Mean Detector
    
    Residuals: 
        mean_: the mean of signal values seen so far
        diff_: the difference between new value and mean_
    
    Stopping Rule:
        Stop if diff_ exceeds some threshold percentage value. 
        Default is 5%.     
    """

    def __init__(self, threshold=0.05):
        super(MeanDetector, self).__init__()

        # Save hyper-parameter(s)
        self.threshold = threshold

        # Required Attributes
        self.total_val = 0  # Used for calculating mean

        # new residuals(s)
        self.diff_ = np.nan

    def update_residuals(self, new_signal_value):
        self._update_base_residuals(new_signal_value)

        # Update attributes
        self.total_val += new_signal_value

        # Update residuals
        self.mean_ = self.total_val / self.signal_size
        self.diff_ = np.absolute(self.mean_ - new_signal_value)

    def check_stopping_rules(self, new_signal_value):
        # check if new value is more than % different from mean
        threshold_level = self.mean_ * self.threshold

        if self.diff_ > threshold_level:
            self.rules_triggered = True

if __name__ == '__main__':
    detector = MeanDetector(threshold=0.05)
    OnlineSimulator(detector, signal).run()
    plot_signal_and_residuals(signal=signal, residuals=detector.residuals_, stop_point=detector.rules_triggered)
