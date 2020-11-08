! Este programa ordena un arreglo de numeros reales
      REAL*8                :: X(20000),a
      INTEGER             :: K,I,J,io
      CHARACTER*80 :: arga
!      DO K=1,10
!           PRINT "(1x,'Introduce el elemento X(',I2,')')", K
!	   call getarg (K,arga)
!           READ(arga,*) X(K)
!     ENDDO
!    El siguiente do se encargara de ordenarlos

OPEN(1, file = "NumerosAli.dat", status = 'old')
DO K=1,20000
	READ(1, *, IOSTAT= io)x(K) 
	IF (io/=0)EXIT
	END DO

     DO I=1,19999
         DO J=1,19999
        
         IF (X(J).GT.X(J+1)) THEN
            a = X(J)
            X(J) = X(J+1)
            X(J+1) = a
         END IF
         END DO  
      END DO

! Se imprime el arreglo
!      PRINT*, X
END PROGRAM 

