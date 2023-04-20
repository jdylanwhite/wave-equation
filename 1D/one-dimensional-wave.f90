program one_dimensional_wave

  ! Ensure all variables are explicitly declared
  implicit none

  ! Define constants
  integer, parameter :: nx = 1000 ! Number of grid points in x-direction
  integer, parameter :: nt = 1000 ! Number of timesteps to integrate
  real, parameter :: dx = 0.01    ! Grid spacing in x-direction
  real, parameter :: dt = 0.01    ! Time step
  real, parameter :: c = 1.0      ! Wave speed
  real, parameter :: sigma = 1.0  ! Width of Gaussian pulse
  real, parameter :: xc = 5.0     ! Where the Gaussian pulse is centered
  
  ! Define variables
  integer :: i, n                                  ! Loop counters
  real :: t                                        ! Float for t during the loop
  real :: x(nx)                                    ! Array for spatial coordinates
  real :: u(nx), u_new(nx), u_old(nx)              ! Wave amplitude at current and next/previous time steps
  real :: s(nx)                                    ! Wave amplitude source
  character(30) :: filePathX, filePathT, filePathU ! Output file path to write data
  integer :: unitX, unitT, unitU                   ! File unit numbers
  
  ! Loop through x and define the grid and initial Gaussian pulse
  do i = 1, nx

     ! Set the spatial coordinate
     x(i) = (i-1)*dx

     ! Gaussian pulse centered at xc
     u(i) = exp(-0.5*((x(i)-xc)/sigma)**2)

     ! Non-dispersive Gaussian pulse centered at xc
     !u(i) = sin(2.0 * 3.14159 * (x(i) - c * 0.0))     
     
     ! Wave packet pulse centered at xc
     !u(i) = sin(4.0*3.14159*x(i))*exp(-0.5*((x(i)-xc)/sigma)**2)
     
     ! Top hat function centered at xc
     !if ((x(i) > (0.75*xc)) .and. (x(i) < (1.25*xc))) then
     !   u(i) = 1
     !else
     !   u(i) = 0
     !end if

     ! Sinusoidal wave centered at xc
     !if ((x(i) > (0.5*xc)) .and. (x(i) < (1.5*xc))) then
     !   u(i) = sin((2*3.14159/xc)*(x(i)-xc))
     !else
     !   u(i) = 0
     !end if

     ! Sinusoidal wave filling the domain
     !u(i) = sin((2.0*3.14159*x(i))/(0.25*dx*(nx-1)))
     
  end do

  ! Apply Dirichlet boundary condition
  u(1) = 0.0
  u(nx) = 0.0

  ! Apply Neumann boundary condition
  !u(1) = u(2)
  !u(nx) = u(nx-1)

  ! Apply the zero-gradient initial condition
  u_old = u
  
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
     u_new(2:nx-1) = 2*u(2:nx-1)
     u_new(2:nx-1) = u_new(2:nx-1) - u_old(2:nx-1)
     u_new(2:nx-1) = u_new(2:nx-1) + ((c*dt/dx)**2)*(u(3:nx)-(2*u(2:nx-1))+u(1:nx-2))

     ! Apply Dirichlet boundary conditions
     u_new(1) = 0
     u_new(nx) = 0

     ! Apply Neumann boundary condition
     !u_new(1) = u_new(2)
     !u_new(nx) = u_new(nx-1)

     ! Update the wave amplitude at the current time step
     u_old = u
     u = u_new
        
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
  
end program one_dimensional_wave
