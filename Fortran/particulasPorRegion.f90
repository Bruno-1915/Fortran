!*******************************************+*************************
!Este programa obtiene cuantas particulas por región hay en una caja *
!También las agrupa dado su centro de masa y da sus propiedades como *
!la velocidad, etc                                                   *
!*********************************************************************

PROGRAM TAREA_OPCIONAL
IMPLICIT NONE
REAL, DIMENSION(:), ALLOCATABLE			:: X,Y,VX,VY,MASA,V 
REAL*8						:: X_CM,Y_CM, MC,MASS,VX_CM,VY_CM
REAL*16						:: STEP_X,STEP_Y,T_X,T_Y,M_X,M_Y,R,R_CM,VMX,VMY
INTEGER						:: m, OI, IO, i,j,k,l,ll,N1,N2,p,pp,iii,jjj,lll,kkk,ooo,qq
CHARACTER*80					:: filename 


PRINT*, '¿Cuantas regiones por renglon seran?'
READ(*,*) N1
PRINT*, '¿Cuantas regiones por columna seran?'
READ(*,*) N2
PRINT*, '¿Cual es el nombre del archivo?'
READ(*,*) filename

OPEN(1, file = filename, status = 'old')

DO 
	READ(1, *, IOSTAT = oi)
	IF (oi/=0) EXIT
	m = m + 1
END DO

ALLOCATE(X(m), Y(m), VX(m), VY(m), MASA(m),V(m))

CLOSE(1)

OPEN(2, file = filename, status = 'old')

DO k=0, m
	READ(2, *, IOSTAT = IO) X(k), Y(k), VX(k), VY(k), MASA(k)
END DO
CLOSE(2)
T_X = MAXVAL(X)
M_X = MINVAL(X)
T_Y = MAXVAL(Y)
M_Y = MINVAL(Y)
STEP_X = ((T_X - M_X)/N1) +.00001
STEP_y = ((T_Y - M_Y)/N2) +.00001
pp=0
OPEN(3, file = "resultado", status = 'unknown')
DO i=0, N2-1
   DO j=0, N1-1
	VY_CM=0.0
	VX_CM=0.0
	X_CM=0.0
	Y_CM=0.0
	p=0.0
	MASS=0.0
	!VMX=0.0
	!VMY=0.0
	WRITE(3,*) 'Los siguientes datos son de la region',i,j
	DO l=1,m
		IF ( (M_Y + STEP_Y*i <= Y(l)) .AND. (Y(l) < M_Y + STEP_Y*(i+1)) ) THEN
		IF ( (M_X + STEP_X*j <= X(l)) .AND. (X(l) < M_X + STEP_X*(j+1)) ) THEN
			p=p+1
		END IF
		END IF		
	END DO
	
       	DO ll=1,m
		IF( (M_Y + STEP_Y*i <= Y(ll)) .AND. (Y(ll) < M_Y + STEP_Y*(i+1)) ) THEN
		IF( (M_X + STEP_X*i <= X(ll)) .AND. (X(ll) < M_X + STEP_X*(i+1)) ) THEN
			X_CM = X_CM + ( X(ll)*MASA(ll)  )
			Y_CM = Y_CM + ( Y(ll)*MASA(ll)  )
			MC = MC + MASA(ll)
			VY_CM = VY_CM + ( VY(ll)*MASA(ll)  )
			VX_CM = VX_CM + ( VX(ll)*MASA(ll) )
			!VMX = VMX + VX(ll)
			!VMY = VMY + VY(ll)
		END IF
		END IF
	END DO
	!R = SQRT((VMX/P)**2 + (VMY/P)**2)
	R_CM = SQRT(((VX_CM/MC)**2) + ((VY_CM/MC)**2))
	WRITE(3,*)'El centro de masa es: ', '(',X_CM/MC,',',Y_CM/MC,')'
	WRITE(3,*)'Su velocidad es: ', '(',VX_CM/MC, ',',VY_CM/MC,')'
	WRITE(3,*)'La rapidez del centro de masa es: ', R_CM 
	!WRITE(3,*)'La velocidad media de las particulas en la region es: ','(',VMX/P,',',VMY/P,')'
	WRITE(3,*)'La rapidez media de las particulas en la region es: ',R
	WRITE(3,*)'La cantidad de particulas en esta region fueron: ', p
	WRITE(3,*)''
	WRITE(3,*) "##############################################################################################"
	WRITE(3,*) ''
	!WRITE(3,*)P,PP
	pp= pp + p
   END DO
END DO	
WRITE(3,*) "##############################################################################################"
WRITE(3,*) 'El total de particulas en las regiones es: ',pp
WRITE(3,*) 'El total de particulas en el archivo es: ', m
CLOSE(3)

END PROGRAM

