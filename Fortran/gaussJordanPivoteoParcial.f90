PROGRAM PIVOTEO_PARCIAL

INTEGER             ::      i,j,k,l,m,nr,nc,indice,Num,io   
CHARACTER*80        ::      filename
REAL*8              ::      A(1000,1000), V(1000), Max,factor(1000), suma, Vi(1000)
REAL*8              ::      pivote,Aux(1000)
REAL*8              ::      X(1000), Y(1000)


PRINT*, "Ingrese el nombre del archivo con la matriz, recuerda que debe ser una matriz aumentada"
READ(*,*) filename
PRINT*, "Ingresa el numero de renglones"
READ(*,*) nr
nc=nr+1

OPEN(2,file=filename,status="OLD")

DO i=1,nr
   READ(2,*)(A(i,j),j=1,nc)
END DO
CLOSE(2)

PRINT*,"La matriz que se leyo fue: "

DO m=1,nr
   DO l=1,nc
      WRITE(*,13,advance="no") A(m,l) ,"  "
   END DO
   WRITE(*,*)
END DO

!!!!!!!!!!!!!!!! Diagonaliza !!!!!!!!!!!!!!!!!!!!!!!!

DO i=1,nc-1
DO j=i,nr
   V(j)=abs(A(j,i))  
END DO


Num=nr
x=v 
Y=X

DO k=1,Num-1
   DO l=i,Num-k
      IF (X(l).GT.X(l+1)) THEN
            HOLD = X(l)
            X(l) = X(l+1)
            X(l+1) = HOLD
      END IF
   END DO
END DO

Max=x(Num)   

DO k=i,Num   
      IF (v(k).eq.Max) THEN
         indice=k               !Éste es el indice del pivote
      END IF
END DO

DO j=i,nr
   Vi(j)=abs(A(i,j))  
END DO


Num=nr
x=vi 

DO k=1,Num-1
   DO l=i,Num-k
      IF (X(l).GT.X(l+1)) THEN
            HOLD = X(l)
            X(l) = X(l+1)
            X(l+1) = HOLD
      END IF
   END DO
END DO

S=x(Num) !Factor a escalar

DO k=1,nc
   A(i,k)=A(i,k)/S   
END DO

!!!!!!!!!!!!!Intercambio de renglones!!!!!!!!!!!!!!!!!

DO k=i,nc
      Aux(k)=A(i,k)
      A(i,k)=A(indice,k)
      A(indice,k)=Aux(k)
END DO

DO k=i,nc 
   A(i,k)=A(i,k)/Max
END DO

PRINT*,
PRINT*,"El siguiente paso resulto en: "
PRINT*,
DO m=1,nr
   DO l=1,nc
      WRITE(*,13,advance="no") A(m,l) ,"  "
   END DO
   WRITE(*,*)
END DO
PRINT*,
PRINT*,"El siguiente paso resulto en: "

   DO j=i+1,nr
      factor(j)=A(j,i)/A(i,i)  
      DO k=1,nc
         A(j,k) = A(j,k) - (factor(j)*A(i,k))
      END DO
   END DO

   DO m=1,nr                                        
      DO l=1,nc                                   
         WRITE(*,13,advance="no") A(m,l) ,"  "     
      END DO
      WRITE(*,*)
   END DO
END DO

DO i=1,nr   
   IF(A(i,i).eq.0) THEN
      PRINT*, "No existe solucion unica"
      STOP
   END IF
END DO

PRINT*,"Las soluciones son: "

X(nr)=A(nr,nr+1)/A(nr,nr)       
WRITE(*,14) "x(",nr,")=",x(nr)
DO i=nr-1,1,-1               
   suma=0                   
   DO j=i+1,nr
      suma=suma+A(i,j)*X(j)
   END DO

   x(i)=(A(i,nr+1)-suma)/(A(i,i))
   WRITE(*,14) "x(",i,")=", x(i)

END DO
13 FORMAT (f8.4,A)
14 FORMAT (A2,I2,A,F10.4)

END PROGRAM

