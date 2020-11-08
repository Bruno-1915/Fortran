PROGRAM Gauss_Jordan
  REAL*8, DIMENSION(:,:), ALLOCATABLE ::  A
  INTEGER                                 ::  i, j
  REAL*8, DIMENSION(:), ALLOCATABLE   ::  temp,X, x0, y,Z
  REAL*8 :: suma,suma1,TOL,w
  CHARACTER*80   :: filename
  PRINT "(A)", "Si quiere una matriz de tamaño n cambie el programa a n= número deseado"

  PRINT*, "Ingrese el nombre del archivo con la matriz"
  READ(*,*) filename
  PRINT "(A)", "Introduzca numero de renglones"
  READ *, n
  ALLOCATE (a(n,n+1), temp(n+1),x(n),x0(n), y(n))

  OPEN(2,file=filename,status="old")

  DO i = 1, n
     READ (2,*) (A(i,j), j=1,N+1)
  END DO
  CLOSE(2)

  PRINT*,"Cual es el valor de omega"
  READ(*,*) w

TOL = 0.00001
x0 = 0.0
y = 1
x = 0.0

DO k=1,100
IF  (maxval(abs(y-x0)) > TOL ) then
  DO i=1,n
   DO j=i+1,n
    !IF (j /= i) THEN
     suma = suma + A(i,j)*x0(j)
    !ENDIF
   ENDDO
   DO j=1,i-1
       suma1 = suma1 + A(i,j)*x(j)
   END DO
    x(i)=w*((A(i,n+1)-suma-suma1))/A(i,i) + (1-w)*x(i)
   suma = 0.0
   suma1 = 0.0
  ENDDO
  ENDIF
  y = x0
  x0 = x
ENDDO

Print*, X

END PROGRAM Gauss_Jordan




