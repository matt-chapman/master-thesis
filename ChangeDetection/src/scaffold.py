# -*- coding: utf-8 -*-

import matplotlib.pyplot as plt
import numpy as np
from collections import defaultdict

np.set_printoptions(precision=3, suppress=True)

"""
Code adapted from Aman Ahuja (https://github.com/amanahuja/change-detection-tutorial/)
"""

def dict_to_arrays(ddict):
    """
    Convenience function used by online simulator to bundle 
    residuals into a dict before returning
    """
    new_dict = {}
    for k,v in ddict.iteritems():
        new_dict[k] = np.array(v)
    return new_dict


def online_simulator(self, signal, change_detector):
    """
    Function that simulates an online streaming scenario for change detection experiments.
    --- 
    Given a signal and a change detector, this simulator passes one signal data point at a time to
    the change detector and processes the results. 
    
    inputs
    ------------------------
    signal: np.array
    change_detector: class change_detector
    
    """
    # Initiate
    # change_detector = cd_algorithm()

    all_residuals = defaultdict(list)

    xx = 0
    # Iterate through the signal, passing data points to the algorithm.
    for value in signal:

        # calculate residuals, compare residuals with the stopping rule(s)
        check_results = next(change_detector.step(value))

        # process results
        rule_triggered = check_results[0]
        res = check_results[1]

        # store residuals
        for k,v in res.iteritems():
            all_residuals[k].append(v)

        if rule_triggered:
            # stopping rule was triggered
            return True, self.dict_to_arrays(all_residuals)

            # Rule wasn't triggered by end of signal
    return False, self.dict_to_arrays(all_residuals)


def run_online_simulation(signal, change_detector, scale=True):
        """Run simulation and print results"""

        # Run simulation
        results = online_simulator(signal, change_detector)

        # Display results
        print_sim_results(signal, results, scale=scale)

        # Return residuals
        residuals = results[1]
        return residuals

def print_sim_results(signal, results, **kwargs):
    """
    Another convenience function to print out the results of our experiment. 
    """
    # Get results
    stopped = results[0]
    residuals = results[1]
    print "Residuals: {}".format([res for res in residuals.viewkeys()])

    if stopped:
        # length of residuals array tells us when the rule was triggered
        stop_point = len(residuals.itervalues().next())

        print "Change detected. Stopping Rule triggered at {}.\n".format(stop_point)
        plot_signal_and_residuals(signal, residuals, stop_point, **kwargs)
    else:
        print "Stopping rule not triggered."
        plot_signal_and_residuals(signal, residuals, **kwargs)


def plot_signal_and_residuals(signal, residuals=None, stop_point=None, scale=True):
    """Convenience function to generate plots of the signal and the residuals"""

    if residuals is None:
        plotcount = 1
    else:
        plotcount = 1 + len(residuals)

    fig, axes = plt.subplots(nrows=plotcount,
                             ncols = 1,
                             sharex=True,
                             figsize=(6, plotcount*3)
                             )

    # First plot the signal
    if plotcount > 1:
        ax = axes[0]
    elif plotcount == 1:
        ax = axes

    ax.plot(signal)
    ax.set_title('Signal')

    # Scale signal
    ax.set_ylim(signal.min()*.5, signal.max()*1.5)
    ax.set_xlim(0, len(signal))

    # Plot a horizontal line where the stop_point is indicated
    if stop_point is not None:
        assert (stop_point > 0) & (stop_point < len(signal))
        ax.vlines(x=stop_point, ymin=0, ymax=ax.get_ylim()[1],
                  colors='r', linestyles='dotted')

    # Now plot each residual
    if residuals is not None:
        for ii, (res_name, res_values) in enumerate(residuals.iteritems()):
            ax = axes[ii+1]
            ax.plot(res_values)
            ax.set_title("Residual #{}: {}".format(ii+1, res_name))
            if scale:
                ax.set_ylim(res_values.min()*0.5, res_values.max() * 1.5)
            ax.vlines(x=stop_point, ymin=0, ymax=ax.get_ylim()[1],
                      colors='r', linestyles='dotted')