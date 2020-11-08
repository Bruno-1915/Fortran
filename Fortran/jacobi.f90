PROGRAM Metodo_Jacobi
!************************************************************************
!Programa que resuelve el sistema Ax=b por el metodo de eliminacion Gaussiana
!falta implementar metodo de pivoteo parcial y pivoteo parcial escalado
!asi como hacerlo una subrutina. Esta escrito en f90
!*************************************************************************
  REAL*8, DIMENSION(:,:), ALLOCATABLE ::  A,B
  INTEGER                             ::  i, j
  REAL*8, DIMENSION(:), ALLOCATABLE   ::  temp,X,x0
  CHARACTER*80                        ::  filename  

  PRINT*, "Ingrese el nombre del archivo con la matriz"
  READ(*,*) filename
  PRINT "(A)", "Introduzca numero de renglones"
  READ *, n
!****************fin de declaraciones *******************************
  ALLOCATE (a(n,n+1), temp(n+1),x(n),B(n,n+1),x0(n))
   
  OPEN(2,file=filename,status="old")

  DO i = 1, n
     READ (2,*) (A(i,j), j=1,N+1)
  END DO
  CLOSE(2)
 B=A   
CALL jacobi(A,n,x)
Print*,'Las soluciones son: ', X
END PROGRAM Metodo_Jacobi
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 

SUBROUTINE jacobi(A,n,x)
REAL*8, DIMENSION(n,n+1)             ::  A
integer                                   :: i,j,k,Nmax
REAL*8, DIMENSION(n+1)                  ::  temp
REAL*8, DIMENSION(n)                  ::  x, x0
REAL*8                              ::   suma

x0=0                                                 
Nmax=800
k=1                                                  
IF (k< Nmax ) THEN                                 
	DO i=1,n
	suma=0		
		DO j=1,n
			IF(j/=i) THEN
				suma=suma+(A(i,j)*x0(j))
	
			END IF
		END DO
		x(i)=(A(i,n+1)-suma)/A(i,i)	     
	
	END DO
	IF(MAXVAL(ABS(x-x0))<0.00000001) THEN
		PRINT*, 'La solucion es: ',X
	STOP
	END IF                                  
	X0=X
	k=k+1

ELSE
print*, 'Se sobrepaso la tolerancia'
END IF

END SUBROUTINE jacobi

