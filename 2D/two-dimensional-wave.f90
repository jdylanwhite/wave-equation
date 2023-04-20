program gaussian_wave

  ! Ensure all variables are explicitly declared
  implicit none

  ! Define constants
  integer, parameter :: nx = 100  ! Number of grid points in x-direction
  integer, parameter :: ny = 100  ! Number of grid points in y-direction
  integer, parameter :: nt = 1000 ! Number of timesteps to integrate
  real, parameter :: dx = 0.1     ! Grid spacing in x-direction
  real, parameter :: dy = 0.1     ! Grid spacing in y-direction
  real, parameter :: dt = 0.01    ! Time step
  real, parameter :: cx = 1.0     ! Wave speed in x-direction
  real, parameter :: cy = 1.0     ! Wave speed in y-direction 
  real, parameter :: sigmax = 1.0 ! Width of Gaussian pulse in x-direction
  real, parameter :: sigmay = 1.0 ! Width of Gaussian pulse in y-direction
  real, parameter :: xc = 5.0     ! Where the Gaussian pulse is centered in x-direction
  real, parameter :: yc = 5.0     ! Where the Gaussian pulse is centered in y-direction
  
  ! Define variables
  integer :: i, n                                  ! Loop counters
  real :: t                                        ! Float for t during the loop
  real :: x(nx), u(nx), u_new(nx)                  ! Arrays for spatial coordinates, wave amplitude at current and next time step
  character(30) :: filePathX, filePathT, filePathU ! Output file path to write data
  integer :: unitX, unitT, unitU                   ! File unit numbers
  
  ! Loop through x and define the grid and initial Gaussian pulse
  do i = 1, nx

     ! Set the spatial coordinate
     x(i) = (i-1)*dx

     ! Gaussian pulse centered at xc
     u(i) = exp(-0.5*((x(i)-xc)/sigma)**2)
     
  end do

  ! Apply Neumann boundary condition
  u(1) = u(2)
  u(nx) = u(nx-1)

  ! Set output file paths
  filePathX = "./data/x.dat"
  filePathT = "./data/t.dat"
  filePathU = "./data/u.dat"

  ! Set output file units
  unitX = 10
  unitT = 11
  unitU = 12

  ! Open files for writing
  open(unitX, file=filePathX, status="replace")
  open(unitT, file=filePathT, status="replace")
  open(unitU, file=filePathU, status="replace")
  
  ! Go ahead and write x data to file and close it
  write(unitX,*) x
  close(unitX)

  ! Write the initial time and u data
  write(unitT,*) 0.0
  write(unitU,*) u
  
  ! Loop through the number of steps and update the Gaussian pulse
  do n = 1, nt

     ! Get the current time
     t = dt*n
     
     ! Update u using the wave equation 
     u_new(2:nx-1) = u(2:nx-1) - c*dt/dx*(u(2:nx-1) - u(1:nx-2))
     
     ! Update the wave amplitude at the current time step
     u = u_new

     ! Apply Neumann boundary condition
     u(1) = u(2)
     u(nx) = u(nx-1)
     
     ! Output data every 10 time steps
     if (mod(n,10) == 0) then

        ! Write u and t data to file
        write(unitT,*) t
        write(unitU,*) u
        
     end if
     
  end do

  ! Close the files for u and t output
  close(unitT)
  close(unitU)
  
end program gaussian_wave
