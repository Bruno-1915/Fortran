PROGRAM Gauss_Jordan_con_pivoteo
!************************************************************************
!Programa que resuelve el sistema Ax=b por el metodo de eliminacion Gaussiana
!falta implementar metodo de pivoteo parcial y pivoteo parcial escalado
!asi como hacerlo una subrutina. Esta escrito en f90
!*************************************************************************
  REAL*8, DIMENSION(:,:), ALLOCATABLE ::  A,B
  INTEGER                                 ::  i, j
  REAL*8, DIMENSION(:), ALLOCATABLE   ::  temp,X
  PRINT "(A)", "Introduzca numero de columnas"
  READ *, n

!****************fin de declaraciones *******************************
  ALLOCATE (a(n,n+1), temp(n+1),x(n),B(n,n+1))
  ! Obtiene los elementos de la matriz
  PRINT*, ' Escriba los renglones (son de tamaño ',n+1,')'
  DO i = 1, n
     READ (*,*) (A(i,j), j=1,N+1)
  END DO
  !
 B=A   
CALL gauss(A,n,x)
Print*, X
  print*, 'residuo =',B(:,N+1)- MATMUL(B(:,1:N),X)
END PROGRAM Gauss_Jordan_con_pivoteo
  

SUBROUTINE gauss(A,n,x)
REAL*8, DIMENSION(n,n+1)             ::  A
integer                                   :: i,j,P,pivote
REAL*8, DIMENSION(n+1)                  ::  temp
REAL*8, DIMENSION(n)                  ::  x
REAL*8                              ::   suma,m
!*****************Realiza eliminacion Gauss-Jordan*****************
  
DO i = 1, n-1                                        !PASO 1
   P=0
   pivote=a(i,i)   
   DO j = i, n                                       !PASO 2
       IF (abs(A(j,i)) >=  pivote ) THEN 
        P=j
        pivote=a(j,i)
       
       ENDIF
 ENDDO
 IF ( P .eq. 0) print*, 'la solucion no es unica'
 IF ( P .eq. 0) stop                              !PASO 2
 IF ( P  /=  i) THEN                                  !PASO 3
       ! Intercambia el renglon p con el renglon i.
      temp = A(P,:);  A(P,:) = A(I,:);  A(I,:) = temp
 ENDIF
 DO j = i+1, n                                    !PASO 4
    m = A(j,i)/A(i,i)                             !PASO 5 multiplicador para cada renglon
        A(j,:) = A(j,:) - m * A(i,:)                  !PASO 6
     ENDDO
  ENDDO
  IF ( a(n,n) .eq. 0) THEN
      PRINT*,' No existe solucion unica. '
  STOP     
  ENDIF
  !******************Inicia sustitucion hacia atras********************
  X(N)=A(N,N+1)/A(N,N)
  DO I = N-1,1,-1
  suma=0.d0
     DO J = I+1,N
      suma = suma + A(i,j)*X(j)
 ENDDO
  X(i)=(A(i,n+1)-suma)/A(i,i)
  ENDDO
END SUBROUTINE gauss 
