# -*- coding: utf-8 -*-

import numpy as np

"""
Code adapted from Aman Ahuja (https://github.com/amanahuja/change-detection-tutorial/)
"""


class ChangeDetector(object):
    """
    A change detection algorithm. 
    
    The algorithm calculates residuals and updates them for each new value passed. 
    Residuals are checked against stopping rules at each change, yielding either True or False, accordingly. 
    
    """

    def __init__(self):
        # Interim and calculated values
        self.signal_size = 0
        self.total_val = 0

        self.mean_ = np.nan

    def update_residuals(self, new_signal_value):
        # Update residuals
        self.signal_size += 1
        self.total_val += new_signal_value
        self.mean_ = self.total_val / self.signal_size

    def _get_residual_dict(self):
        """create a dictionary of residuals to return. 
        Inclues all class and instance variables ending in '_'
        """
        residuals_dict = {}
        for k,v in self.__dict__.iteritems():
            if k.endswith('_'):
                residuals_dict[k] = v

        return residuals_dict

    def check_stopping_rules(self, new_signal_value):
        rules_triggered = False

        # No rules implemented
        pass

        return rules_triggered

    def _step(self, new_signal_value):

        # update residuals
        self.update_residuals(new_signal_value)

        # compare residuals to stopping_rules
        rules_triggered = self.check_stopping_rules(new_signal_value)

        if rules_triggered:
            yield (True, self._get_residual_dict())

        else:
            yield (False, self._get_residual_dict())

    def step(self, new_signal_value):
        return self._step(new_signal_value)