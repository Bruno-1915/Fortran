PROGRAM READDATA
IMPLICIT NONE

REAL, allocatable :: x(:)
CHARACTER*80 :: filename
INTEGER :: i,oi,io,m
m=0

print*,'¿Cual es el nombre del archivo con los datos?'
read(*,*) filename

OPEN(1, file = filename, status = 'old')

DO 
	READ(1, *, IOSTAT = oi)
	IF (oi/=0) EXIT
	m = m + 1
END DO

allocate(x(m))

close(1)

OPEN(2, FILE = filename, status = 'old')

DO i=1,m
	read(2,*,IOSTAT=io) x(i)
	IF (io > 0) THEN
	WRITE(*,*) 'Algo salio mal con la entrada'
	EXIT
	ELSE IF (io < 0) THEN
	PRINT*, 'No todas las variables recibieron entrada, el promedio y la desviacion no seran adecuadas'
	print*, m
	EXIT 
	END IF
END DO

CLOSE(2)

print*,'El promedio es: ', promedio(x,m)
print*,'La desviacion estandar es: ', Desviacion(x,m)
print*,'La cantidad de datos es de: ',m


!!!!!!!!!  Funciones  !!!!!!!!!!!!!!

CONTAINS
REAL FUNCTION Promedio (x, n)
IMPLICIT NONE
INTEGER, INTENT(IN) :: n
REAL, dimension(n), intent(inout) :: x
REAL(8) :: sum,b
INTEGER :: i
sum=0

DO i=1, n
	sum= sum + x(i)
END DO 
b= sum/m
promedio=b
END FUNCTION Promedio


REAL FUNCTION Desviacion(x,n)
IMPLICIT NONE
INTEGER, INTENT(IN) :: n
REAL, dimension(n), intent(inout) :: x
REAL(8) :: s,suma,suma_1,op_1
INTEGER :: i
suma=0

DO i=1, n
	op_1 = (x(i)-(promedio(x,n)))
	suma = suma + (op_1**2.)
END DO
suma_1 = suma/(n-1)
s= SQRT(suma_1)
Desviacion=s
END FUNCTION Desviacion

END PROGRAM READDATA
