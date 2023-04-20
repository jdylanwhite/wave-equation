# Import modules
import numpy as np
import matplotlib.pyplot as plt
import time

# Load data from output file
xData = np.loadtxt('./data/x.dat')
tData = np.loadtxt('./data/t.dat')
uData = np.loadtxt('./data/u.dat')

# Set up the figure
fig, ax = plt.subplots(figsize=(8,6))

# Loop through data and plot data
for n,t in enumerate(tData):

    # Plot the data
    ax.plot(xData[:-1],uData[n,:-1])

    # Set titles
    ax.set_title(f"Time: {t}")
    ax.set_xlabel('x')
    ax.set_ylabel('Wave amplitude')

    # Set axis limits
    ax.set_xlim(xData[0], xData[-2])
    ax.set_ylim(-1.5, 1.5)
    
    # Pause the plot for a moment
    plt.pause(0.25)

    # Clear the plot to update the results
    if n != len(tData)-1:
        ax.clear()
        
plt.show()
