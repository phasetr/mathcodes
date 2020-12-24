subroutine circular_IBB(D, xc, yc, nxmin, nxmax, nymin, nymax, finout)
 implicit none

 !====================入出力変数=======================================
 real(8), intent(in):: D
 real(8), intent(in):: xc, yc
 integer, intent(in):: nxmin, nxmax, nymin, nymax
 real(8), intent(inout):: finout(1:9, nxmin:nxmax, nymin:nymax)
 !====================================================================
 !====================IBB用定数=======================================
 real(8), parameter:: pi = dacos(-1.d0)	!円周率

 real(8), parameter:: ds = 1.d0		!格子間隔（lattice unit）
 real(8), parameter:: rho0 = 1.d0	!代表密度
 !====================================================================
 !====================IBB用変数=======================================
 integer:: nb_sup 			!境界点数上限
 integer:: nb				!境界点数（真値）

 integer, allocatable:: xf(:), yf(:)		!流体点（格子点）
 integer, allocatable:: xb(:), yb(:)		!物体点（格子点）

 real(8), allocatable:: xw(:), yw(:)		!境界点
 real(8), allocatable:: delta(:)		!流体点と境界点の距離
 integer, allocatable:: dir(:)			!bounce-backの方向
 real(8):: fw, fwbb			!境界点の分布関数とそのbounce-back
 real(8), allocatable:: ftemp(:)

 real(8), allocatable:: ud(:), vd(:)	!境界で満たすべき流速
 !====================================================================
 !=====================その他定数・変数===============================
 !粒子速度
 real(8), parameter::    cx(9) = (/ 0.d0, 1.d0, 0.d0, -1.d0,  0.d0,  1.d0, -1.d0, -1.d0,  1.d0 /)
 real(8), parameter::    cy(9) = (/ 0.d0, 0.d0, 1.d0,  0.d0, -1.d0,  1.d0,  1.d0, -1.d0, -1.d0 /) 

 integer, parameter::    ci(9) = (/ 0, 1, 0, -1,  0,  1, -1, -1,  1 /)
 integer, parameter::    cj(9) = (/ 0, 0, 1,  0, -1,  1,  1, -1, -1 /) 

 !係数
 real(8), parameter::    E(9) = (/ 4.d0/ 9.d0, 1.d0/ 9.d0, 1.d0/ 9.d0, 1.d0/ 9.d0, 1.d0/ 9.d0 ,&
                                1.d0/36.d0, 1.d0/36.d0, 1.d0/36.d0, 1.d0/36.d0 /)

 integer:: i, j, k, l			!インデックス変数
 real(8):: s				!媒介変数
 real(8), parameter:: epsilon = 1.d-14	!境界に近い格子点の判定のための閾値
 !====================================================================
 !-----------値の代入------------------
 nb_sup = 10*(int(D)+1)	!境界点数上限（多めに定義）
 !-------------------------------------
 !======================allocate======================================
 allocate(xf(1:nb_sup))
 allocate(yf(1:nb_sup))
 allocate(xw(1:nb_sup))
 allocate(yw(1:nb_sup))
 allocate(xb(1:nb_sup))
 allocate(yb(1:nb_sup))
 allocate(delta(1:nb_sup))
 allocate(dir(1:nb_sup))
 allocate(ftemp(1:nb_sup))
 allocate(ud(1:nb_sup))
 allocate(vd(1:nb_sup))
 !====================================================================
 

 !========================境界点探索==================================
 k = 0

 do j = nymin, nymax
  do i = nxmin, nxmax


   !境界点 /= 格子点の場合
   if(dabs(inner(i,j)) > epsilon) then

    do l=2,9
     if((inner(i,j) > 0.d0).and.(inner(i+ci(l), j+cj(l)) < 0.d0)) then
      k=k+1

      xf(k) = i
      yf(k) = j 
      xb(k) = i+ci(l)
      yb(k) = j+cj(l)

      s = ( - dble((xb(k)-xc)*(xf(k)-xb(k)) + (yb(k)-yc)*(yf(k)-yb(k))) &
            + dsqrt(dble((xb(k)-xc)*(xf(k)-xb(k)) + (yb(k)-yc)*(yf(k)-yb(k)))**2-dble((xf(k)-xb(k))**2+(yf(k)-yb(k))**2)*dble((xb(k)-xc)**2+(yb(k)-yc)**2 - (0.5d0*D)**2) ) )&
          / dble((xf(k)-xb(k))**2+(yf(k)-yb(k))**2)

      xw(k) = dble(xb(k)) + dble(xf(k)-xb(k))*s
      yw(k) = dble(yb(k)) + dble(yf(k)-yb(k))*s
    
      delta(k) = dsqrt(dble(xf(k)-xw(k))**2+dble(yf(k)-yw(k))**2)/dsqrt(dble(xf(k)-xb(k))**2+dble(yf(k)-yb(k))**2)
      dir(k) = l

      ud(k) = 0.d0
      vd(k) = 0.d0
     end if
    end do


   !境界点 = 格子点の場合
   else

    do l=2,9
     if((inner(i-ci(l), j-cj(l)) > 0.d0).and.(inner(i+ci(l), j+cj(l)) < 0.d0)) then
      k=k+1

      xf(k) = i
      yf(k) = j
      xb(k) = i
      yb(k) = j
      xw(k) = i*ds
      yw(k) = j*ds
    
      delta(k) = 0.d0
      dir(k) = l

      ud(k) = 0.d0
      vd(k) = 0.d0
     end if
    end do

   end if

  end do
 end do

 nb = k
 !====================================================================
 !========================改良bounce back==============================
 do k=1, nb
  fw = (1.d0-delta(k))*finout(dir(k),xf(k),yf(k)) + delta(k)*finout(dir(k),xb(k),yb(k))
  fwbb = fw - 6.d0*E(dir(k))*(cx(dir(k))*ud(k)+cy(dir(k))*vd(k))
  ftemp(k) = 1.d0/(1.d0 + delta(k))*fwbb + delta(k)/(1.d0 + delta(k))*finout(bb(dir(k)),xf(k)+ci(bb(dir(k))),yf(k)+cj(bb(dir(k))))
 end do

 do k=1,nb
  finout(bb(dir(k)),xf(k),yf(k)) = ftemp(k)
 end do
 !====================================================================
 !======================deallocate====================================
 deallocate(xf)
 deallocate(yf)
 deallocate(xw)
 deallocate(yw)
 deallocate(xb)
 deallocate(yb)
 deallocate(delta)
 deallocate(dir)
 deallocate(ud)
 deallocate(vd)
 !====================================================================



 contains
 !===============識別関数===============================================
 function inner(i,j)
  real(8):: inner
  integer, intent(in):: i, j

  inner = (i*ds-xc)**2 + (j*ds-yc)**2 - (0.5d0*D)**2

 end function
 !=====================================================================
 !===============baunce-backの方向=====================================
 function bb(l)
  integer:: bb
  integer, intent(in):: l

  if(l==1) then
   bb = 1
  else if(l==2) then
   bb = 4
  else if(l==3) then
   bb = 5
  else if(l==4) then
   bb = 2
  else if(l==5) then
   bb = 3
  else if(l==6) then
   bb = 8
  else if(l==7) then
   bb = 9
  else if(l==8) then
   bb = 6
  else if(l==9) then
   bb = 7
  end if

 end function
 !=====================================================================

end subroutine circular_IBB