PROGRAM Euler_Cromer
!*********************************************************************
!
!
!
!**********************************************************************
 INTEGER, PARAMETER :: PREC = SELECTED_REAL_KIND(9,99)
 REAL(PREC), DIMENSION(:), ALLOCATABLE :: theta1,theta2,omega,t
 REAL(PREC) :: length,dt
!
 n=100000
 ALLOCATE (theta1(0:n),theta2(0:n),omega(0:n),t(0:n))
!
!
 call inicializa(theta1, omega, t, n, length, dt)
 call calcula (theta1, omega, t, n, length, dt)
! call calcula (theta2, omega, t, n, length, dt)
 call despliega (theta1, omega, t, n, length, dt)
!

END PROGRAM Euler_Cromer
!
!
SUBROUTINE inicializa(theta1, omega, t, n ,length, dt)
 INTEGER, PARAMETER :: PREC = SELECTED_REAL_KIND(9,99)
 INTEGER, INTENT (IN) :: n
 REAL(PREC), DIMENSION(0:n) :: theta1,theta2,omega,t
 REAL(PREC) :: length,dt
 print*,'Angulo inicial del pendulo 1 (en radianes)'
 read*, theta1(0)
 print*,'Angulo inicial del pendulo 2 (en radianes)'
 read*, theta2(0)
 print*,'Velocidad angular inicial del pendulo (en radianes/s)'
 read*, omega(0)
 t(0)=0.
 print*,'Longitud del pendulo (in m)'
 read*, length
 print*, 'Tamaño de paso (en segundos)'
 read*, dt
END SUBROUTINE inicializa
!
!
SUBROUTINE calcula(theta, omega, t, n, length, dt)
 INTEGER, PARAMETER :: PREC = SELECTED_REAL_KIND(9,99)
 INTEGER, INTENT (IN) :: n
 REAL(PREC), DIMENSION(0:n) :: theta,omega,t
 REAL(PREC) :: length,dt,g,periodo
 INTEGER :: i
 PI= 4.*ATAN(1.)
 i= 0
 g= 9.80
 q=1/2.0
 PRINT*,'¿Cual es el valor de la fuerza?'
 Read(*,*) df
 dfr=2/3.0
 periodo= 2.0*PI/ SQRT(g/length) !Periodo del pendulo
 DO
 omega(i+1) = omega(i) - (g/length) *sin(theta(i)) * dt - q * omega(i)*dt+df*sin(dfr*t(i))*dt
 theta(i+1) = theta(i) + omega(i+1) * dt ! Metodo de Cromer si descomento
 if (theta(i+1) > PI ) theta(i+1)=theta(i+1)-2.*PI
 if (theta(i+1) < -PI) theta(i+1)=theta(i+1)+2.*PI
 t(i+1) = t(i) + dt
! IF (t(i+1) >= 60 * periodo) EXIT
 IF (i >= n-1) EXIT
 i=i+1
 ENDDO
END SUBROUTINE calcula
!
!
SUBROUTINE despliega(theta, omega, t, n, length, dt)
 INTEGER, PARAMETER :: PREC = SELECTED_REAL_KIND(9,99)
 INTEGER, INTENT (IN) :: n
 REAL(PREC), DIMENSION(0:n) :: theta,omega,t
 REAL(PREC) :: length,dt
 INTEGER :: i
 CHARACTER(LEN=10), PARAMETER :: f1 = '(3ES16.6)'
 OPEN (UNIT=10,FILE='P.dat',STATUS='UNKNOWN')
 !
 DO i=0,N
 DO NU=0,N
 IF ( ABS(t(i)-((2*NU*pi)/(2/3))) < DT/2 ) THEN 
 WRITE(10,f1)theta(i),omega(i),t(i)
 END IF
 END DO 
 END DO
!
 CLOSE(10)
END SUBROUTINE despliega
