import matplotlib.pyplot as plt
import numpy as np

from collections import defaultdict
np.random.seed(seed=111111)
np.set_printoptions(precision=3, suppress=True)

signal = np.random.normal(scale=1, size=200)
signal[:50] += 50; signal[50:] += 55; signal[150:] -= 20
signal[110] = 0

plt.plot(signal)
plt.ylim(0,100)
plt.title("An example signal")
plt.show()
