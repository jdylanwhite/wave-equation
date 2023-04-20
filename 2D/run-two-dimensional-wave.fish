#!/usr/bin/env fish

# Compile the script
echo Compiling...
gfortran -o one-dimensional-wave one-dimensional-wave.f90

# Run the model
echo Running...
./one-dimensional-wave

echo Done!