! Este programa ordena un arreglo de numeros reales
      character*80::arga
      REAL                ::a,b, X(20000)
      INTEGER             :: K,I,J,N,io
!N=1000
!call getarg(1, arga)
!read(arga,*) N
!Do i=1,n              ! en el do pueden ir variables!!!!!
!   X(i)= a+(b-a)*rand(0) ! operacion para obtener el aleatorio en el intervalo
!end do
!    El siguiente do se encargara de ordenarlos
OPEN(1, file = "NumerosAli.dat", status = 'old')
DO k=1,20000
	Read(1,*,IOSTAT=io)X(K) 
	IF (io/=0)EXIT
	END DO


      DO I=20000,1,-1
         DO J=1,I
        
         IF (X(J).GT.X(J+1)) THEN
            HOLD = X(J)
            X(J) = X(J+1)
            X(J+1) = HOLD
         ENDIF
         ENDDO  
       ENDDO

! Se imprime el arreglo
END PROGRAM 

