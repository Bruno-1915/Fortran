PROGRAM Euler_Cromer
!*************************************************************************
!Pelotas con la misma masa, una sobre la otra, ambas colisionan entre si *
!*************************************************************************
REAL*8, DIMENSION(:), ALLOCATABLE 	:: x1,x2,v1,v2,t
REAL*8 				:: m1,m2,dt,a,b,c
Integer*8 				:: k

n = 1000
k = 1
a = 1.35
b = 0.00015
c = 1.35
ALLOCATE (x1(0:n),x2(0:n),v1(0:n),v2(0:n),t(0:n))
CALL inicializa(x1, x2, v1,v2, t, n, m1, m2, dt)
CALL calcula (x1, x2, v1, v2, t, n, m1, m2, dt)
CALL despliega (x1,x2, v1, v2, t, n, m1, m2, dt)
END PROGRAM Euler_Cromer

SUBROUTINE inicializa(x1, x2, v1, v2, t, n , m1, m2, dt)
INTEGER, INTENT (IN) :: n
REAL*8, DIMENSION(0:n) :: x1,x2,v1,v2,t
REAL*8 :: m1,m2,dt
x1(0)=1.0
x2(0)=3.0
v1(0)=0.0
v2(0)=0.0
t(0)=0.0
m1=1.0
m2=9*m1
dt=0.0003
END SUBROUTINE inicializa
!
!
SUBROUTINE calcula(x1,x2, v1,v2, t, n, m1, m2, dt)
INTEGER, INTENT (IN) :: n
REAL*8, DIMENSION(0:n) :: x1,x2,v1,v2,t
REAL*8 :: length,dt,g,periodo,c,m1,m2,l
INTEGER :: i
i= 0
g=9.81
DO i = 0, n-1
	V1(i+1)=V1(i)-g*t(i)
	x1(i+1)=x1(i)+((V1(i+1)+V1(i))/2.)*dt
	V2(i+1)=V2(i)-g*t(i)
	x2(i+1)=x2(i)+((V2(i+1)+V2(i))/2.0)*dt
	IF ((x2(i+1) - x1(i+1)) < 0.0) THEN
		V1(i+1) =-1*(  (2*m1/(m1+m2))*V1(i+1)+((m2-m1)/(m1+m2))*V2(i+1) )
		V2(i+1) =-1* ( ((m1-m2)/(m1+m2))*V1(i+1)+(2*m2/(m1+m2))*V2(i+1) )
	END IF
	IF (x1(i+1) < 0.0 ) THEN
        	V1(i+1) = -V1(i+1)
	END IF
	t(i+1) = t(i) + dt
END DO

END SUBROUTINE calcula

SUBROUTINE despliega(x1, x2, v1,v2, t, n, m1, m2, dt)
INTEGER, INTENT (IN) :: n
REAL*8, DIMENSION(0:n) :: x1, x2,v1,v2,t
REAL*8 :: length,dt,c,m1,m2
INTEGER :: i,m,j
CHARACTER(LEN=10), PARAMETER :: f1 = '(6ES16.6)'
CHARACTER(20) :: nombre
PI = 4.*ATAN(1.)
dfr=2/3.0 
OPEN (UNIT=10,FILE="datos.dat",STATUS='UNKNOWN')
DO j=0,n
	WRITE(10,f1) x1(j),x2(j),t(j),v1(j),v2(j)
END DO 
CLOSE(10)
OPEN(UNIT=111,FILE="script.gnp",STATUS='UNKNOWN')
WRITE(111,*) 'set terminal png'
WRITE(111,*) 'set output "otro.png"'
WRITE(111,*) 'set multiplot layout 2,1 title "m1=9*m2"'
WRITE(111,*) "set xlabel 'Tiempo'"
WRITE(111,*) "set ylabel 'Posiciones'"
WRITE(111,*) 'plot "datos.dat" u 3:1, "datos.dat" u 3:2'
WRITE(111,*) "set xlabel 'Posicion 2'"
WRITE(111,*) "set ylabel 'Velocidad 2'"
WRITE(111,*) 'plot "datos.dat" u 2:5'
WRITE(111,*) 'unset multiplot'
WRITE(111,*) 'set term x11'
WRITE(111,*) 'set multiplot layout 2,1 title "m1=9*m2"'
WRITE(111,*) "set xlabel 'Tiempo'"
WRITE(111,*) "set ylabel 'Posiciones'"
WRITE(111,*) 'plot "datos.dat" u 3:1, "datos.dat" u 3:2'
WRITE(111,*) "set xlabel 'Posicion 2'"
WRITE(111,*) "set ylabel 'Velocidad 2'"
WRITE(111,*) 'plot "datos.dat" u 2:5'
WRITE(111,*) 'unset multiplot'
WRITE(111,*) 'pause 5'


CLOSE(111)
CALL SYSTEM("gnuplot script.gnp")
END SUBROUTINE despliega
