program difusion

!!!!!!!!!!!!!!!!!!!DECLARACIN DE VARIABLES!!!!!!!!!!!!!!!!
INTEGER		::contador,unidades,centenas,miles,decenas
CHARACTER*11	::FPOS,NOM
real, allocatable, dimension(:)  :: tt, ttnew
n=(63*2)+1
allocate (tt(1:n))
allocate (ttnew(1:n))
l=1.

tau = 0.00001
h = l/real(n-1)
k = 1.
m= 100
!!!!!!!!!!!!!!!!!!!CONDICIN DE ESTABILIDAD!!!!!!!!!!!!!!!!!!!!!

coeff = k*tau/(h**2)

if (coeff<0.5) then
   print*, "la sol es estable", coeff
else
   print*, "la sol es inestable ", coeff
end if

!!!!!!!!!!!!!!!!!!!!CONDICIONES INICIALES !!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!! al tiempo t=0 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
do i=1,n
   tt(i) = 0
end do
tt((n-1)/2) = 1./h
!!!!!!!!!!!!!!!!!!!INTEGRACION!!!!!!!!!!!!!!!!!!!!!

!open(unit=11, file="difusion.dat", status="unknown")
CONTADOR=0
   OPEN(unit=33, file='script.gnp', status= 'unknown')
   WRITE(33,*) 'set yrange[0:80]'
do j=1, m
   
 contador = contador + 1 !Va avanzando en uno energia0001, 0002, etc
 unidades    = MOD(contador, 10)
 decenas    = MOD(contador - unidades,100) / 10
 centenas    = MOD(contador - unidades - decenas, 1000) / 100
 miles    = MOD(contador - unidades - decenas - centenas,10000) / 1000
 nom      = ACHAR(miles + 48) // ACHAR(centenas + 48) // &
              ACHAR(decenas + 48) // ACHAR(unidades + 48)
    nom      = ADJUSTL(nom)
    fpos     = 'difusion' // nom 
 !FPOS=ACHAR(CONTADOR)
  OPEN(unit=31, file=FPOS, status='unknown')

 do k=2, n-1

   ttnew(k) = tt(k) + coeff*(tt(k+1) + tt(k-1) -2*tt(k))
   ttau = j*tau
   write(31,*) k*h, ttau, ttnew(k)
   
 end do
CLOSE(31)

   write(33,*) 'plot "',fpos,'" u 1:3 w l'
   write(33,*) 'pause .3'


 tt= ttnew
end do

CLOSE(33)
CALL SYSTEM ("gnuplot script.gnp")
end program difusion  



