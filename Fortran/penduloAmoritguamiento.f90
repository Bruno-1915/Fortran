PROGRAM Euler_Cromer
!*********************************************************************
!Este programa pretende observar todas las variaciones del péndulo con amortiguamiento
!forzamiento, con y sin aproximación de ángulos pequeños y llegar a observar
!comportamiento caótico.
!**********************************************************************
 !Var_global retain
 REAL*8, DIMENSION(:), ALLOCATABLE :: x1,x2,v1,v2,t
 REAL*8 :: m1,m2,dt,a,b,c
 Integer*8 :: k
!print*, 'Número de puntos: '
!read*, n
! n = 10000
 n = 1000
 k = 1
 a = 1.35
 b = 0.00015
 c = 1.35
 !End_Var
 ALLOCATE (x1(0:n),x2(0:n),v1(0:n),v2(0:n),t(0:n))
!
!
 call inicializa(x1, x2, v1,v2, t, n, m1, m2, dt)
 !do k = 1, 10000
 !do k = 1, 10 
    call calcula (x1, x2, v1, v2, t, n, m1, m2, dt)
    !print*, c
    call despliega (x1,x2, v1, v2, t, n, m1, m2, dt)
    !print*, "lyap",i,"_",c
   ! c = c + b
  ! c = a + i*b
!   i = i + 1
! end do
!
END PROGRAM Euler_Cromer
!
!
SUBROUTINE inicializa(x1, x2, v1, v2, t, n , m1, m2, dt)
!Use VAR_global 
 INTEGER, INTENT (IN) :: n
 REAL*8, DIMENSION(0:n) :: x1,x2,v1,v2,t
 REAL*8 :: m1,m2,dt
 !print*,'Angulo inicial del pendulo (en radianes)'
 x1(0)=1.0
 x2(0)=3.0
 !print*,.'Velocidad angular inicial del pendulo (en radianes/s)'
 v1(0)=0.0
 v2(0)=0.0
 t(0)=0.0
 m1=1.0
 !Aqui van las m
 m2=m1
! m2=2*m1
! m2=9*m1
 !print*,'Longitud del pendulo (en m)'
 !length=9.81
 !print*, 'Tamaño de paso (en segundos)'
 dt=0.0003
END SUBROUTINE inicializa
!
!
SUBROUTINE calcula(x1,x2, v1,v2, t, n, m1, m2, dt)
!Use VAR_global
 INTEGER, INTENT (IN) :: n
 REAL*8, DIMENSION(0:n) :: x1,x2,v1,v2,t
 !,S1,S2
 REAL*8 :: length,dt,g,periodo,c,m1,m2,l
 INTEGER :: i
 i= 0
 g=9.81
 ! V1(1)=V1(0)-g*t(0)
! V2(1)=V2(0)-g*t(0)
!S1(0)=0
!S2(0)=0
! periodo= 2.0*PI/ SQRT(g/length) !Periodo del pendulo
 DO i = 0, n-1
    V1(i+1)=V1(i)-g*t(i)
   ! S1(i+1)=V1(i+1)+V1(i)
    x1(i+1)=x1(i)+((V1(i+1)+V1(i))/2.)*dt
   ! x1(i+1)=x1(i)+V1(i+1)*dt
    V2(i+1)=V2(i)-g*t(i)
   ! S2(i+1)=V2(i+1)+V2(i)
    x2(i+1)=x2(i)+((V2(i+1)+V2(i))/2.0)*dt
   ! x2(i+1)=x2(i)+V2(i+1)*dt
    if (x2(i+1) < x1(i+1)) then
        V2(i+1) =-1*(  (2*m1/(m1+m2))*V1(i+1)+((m2-m1)/(m1+m2))*V2(i+1) )
 !       l=x1(i+1)
  !       x1(i+1)=x1(i+1)-x2(i+1)
!        x1(i+1)=x2(i+1)
!        x2(i+1)=l
!         x2(i+1)=x1(i+1)-x2(i+1)
       V1(i+1) =-1* ( ((m1-m2)/(m1+m2))*V1(i+1)+(2*m2/(m1+m2))*V2(i+1) )
     
    end if
    if (x1(i+1) < 0.0 ) then
        V1(i+1) = -V1(i+1)
!        X1(i+1) = -X1(i+1)
    end if
  !  end if
    t(i+1) = t(i) + dt

 End do
!
END SUBROUTINE calcula
!
!
SUBROUTINE despliega(x1, x2, v1,v2, t, n, m1, m2, dt)
 INTEGER, INTENT (IN) :: n
 REAL*8, DIMENSION(0:n) :: x1, x2,v1,v2,t
 REAL*8 :: length,dt,c,m1,m2
 INTEGER :: i,m,j
 CHARACTER(LEN=10), PARAMETER :: f1 = '(3ES16.6)'
 CHARACTER(20) :: nombre
 !m=0
 PI = 4.*ATAN(1.)
 dfr=2/3.0
 
 !print*, 'nombre archivo de salida: '
 !read*, nombre
nombre = "rebote.data"
 OPEN (UNIT=10,FILE=nombre,STATUS='UNKNOWN')
 OPEN (UNIT=11,FILE="vel.data",STATUS='UNKNOWN')
! OPEN (UNIT=10,FILE=nombre,STATUS='old')

 !
 Do j=0,n
 !   if (abs(t(j) - 2*m*PI/dfr) < dt/2 ) then
 !      m = m+1
       WRITE(10,*) x1(j),x2(j), t(j)
       Write(11,*) V1(j),V2(j)
!    end if
 End Do
 
 CLOSE(10)
 Close(11)
!Call System("gnuplot 'rebote.data' u 3:2 , 'rebote.data' u 3:2")
END SUBROUTINE despliega
