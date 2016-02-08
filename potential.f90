program nuclear_potential
  ! A small program to calculate the potential energy curve (Yukawa & Coulomb)
  ! that descirbes the force governing the nuclear fore.

  implicit none
  integer :: i, yukawa_data, coulomb_data
  real :: r, dr, f, r_0

  ! open files
  yukawa_data  = 60
  coulomb_data = 70
  open(file='yukawa_potential.dat',unit=yukawa_data,status='replace',form='formatted')
  open(file='coulomb_potential.dat',unit=coulomb_data,status='replace',form='formatted')

  i    = 0   ! loop counter
  r    = 0.0 ! intial position
  dr   = 0.1 ! step size
  f    = 1.0 ! interaction stregth
  r_0  = 2.0 ! bohr radius

  ! calculate potential and write to file
  ! then increase radius
  do while (r <= 5)
     write (yukawa_data,*) r, abs(U(r,r_0,f))
     write (coulomb_data,*) r, abs(U_c(r,f))
        r = r + dr
  end do

contains

  ! Yukawa potential energy funcion
  function U(r,r_0,f)
    implicit none
    real, intent (in) :: r,f,r_0
    real :: U

    U = -f**2 * (exp(-r/r_0) / r)

  end function U

  ! Coulomb potential energy function
  function U_c(r,f)
    implicit none
    real, intent(in) :: r,f
    real :: U_c

    U_c = (f**2 / r)

    end function U_c

end program nuclear_potential
