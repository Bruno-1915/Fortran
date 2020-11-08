program adveccion

real:: tau, coeff,c,L,PI,sigma
integer:: N, m, i, j,k,nstep,contador,unidades,decenas,centenas,miles
character*11::fpos,nom

REAL, DIMENSION(:), ALLOCATABLE   ::  x, a, a_new
    INTEGER, DIMENSION(:), ALLOCATABLE::  im,ip

N = 50

ALLOCATE (x(N), a(N),a_new(N),ip(N),im(N))
!! Constantes para la condicion inicial
PI=4.* atan(1.0)
sigma=0.1
k_wave=PI/sigma
!! Para la discretizacion 
tau = 0.0015
L = 1.
h = L / N
c = 1
Nstep = 200

coeff = -c * tau / (2.*h)
coeff2 = ((c**2)*tau**2)/(2.*h**2)
!! Condicion inicial 
 Do i=1,N
   x(i) = (i-0.5)*h - L/2.
   a(i) = cos(k_wave*x(i)) * exp(-x(i)*x(i)/(2.*sigma*sigma))
 enddo
!! condiciones periodicas
 Do i=2,N-1
      ip(i) = i+1
      im(i) = i-1
 enddo
     ip(1)= 2
     im(1)= N

     ip(N)= 1
     im(N)= N-1

!OPEN(UNIT=111,FILE='adveccion.dat',STATUS='UNKNOWN')
CONTADOR=0
   OPEN(unit=33, file='script.gnp', status= 'unknown')
   WRITE(33,*) 'set yrange[-4.5:5]'
   WRITE(33,*) 'set xrange[-.3:.4]'


do i=1,Nstep


 contador = contador + 1 !Va avanzando en uno energia0001, 0002, etc
 unidades    = MOD(contador, 10)
 decenas    = MOD(contador - unidades,100) / 10
 centenas    = MOD(contador - unidades - decenas, 1000) / 100
 miles    = MOD(contador - unidades - decenas - centenas,10000) / 1000
 nom      = ACHAR(miles + 48) // ACHAR(centenas + 48) // &
              ACHAR(decenas + 48) // ACHAR(unidades + 48)
    nom      = ADJUSTL(nom)
    fpos     = 'adveccion' // nom
 !FPOS=ACHAR(CONTADOR)
  OPEN(unit=31, file=FPOS, status='unknown')



do k=1, N
!!!!Lax -Wendor
a_new(k) = a(k) + coeff*(a(ip(k)) - a(k)) + coeff2*( a(ip(k)) - 2*a(k) + a(im(k))  )

!lax chafa
!a_new(k) = (a(ip(k))+a(im(K)))/2 + coeff * (a(ip(k)) -a(im(k)))
!original metodo de diferencias finitas
!a_new(k) = a(k)+ coeff * (a(ip(k)) -a(im(k)))
!write (111,*) x(k), tau*i, a_new(k)
!!!write para la pelicula
write(31,*) x(k), a_new(k)


end do 
close(31)

       a=a_new

   write(33,*) 'plot "',fpos,'" w l'
   write(33,*) 'pause .05'

end do

write(33,*) 'pause 1'
close(33)

call system("gnuplot script.gnp")

end program adveccion

