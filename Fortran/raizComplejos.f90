character*80  ::arga
real*8  ::a(6)
complex*16 ::aa,bb,cc,x,y,z
!print*, 'Ingrese los coeficientes complejos en forma de lista'
call getarg( 1, arga ) 
read(arga,*)a(1)
call getarg( 2, arga )
read(arga,*)a(2)
call getarg( 3, arga )
read(arga,*)a(3)
call getarg( 4, arga )
read(arga,*)a(4)
call getarg( 5, arga )
read(arga,*)a(5)
call getarg( 5,arga )
read(arga,*)a(6)

aa=cmplx(a(1),a(2))
bb=cmplx(a(3),a(4))
bb=cmplx(a(5),a(6))
print *,aa
print*, real(aa),aimag(aa)
print*,cmplx(a(3),a(4))
print*,cmplx(a(5),a(6))



x=sqrt(((bb**2)-(4*aa*cc)))
y=(((-1*bb)+x)/(2*aa))
z=(((-1*bb)-x)/(2*aa))

print*, 'La primer solucion es', y
print*, 'La segunda solucion es',z
print*, 'La comprobacion de la primer solucion es: ', aa*(y**2) + bb*y + cc 
print*, 'La comprobacion de la segunda solucion: ', aa*(z**2) + bb*z + cc



end program 
