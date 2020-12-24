subroutine circular_IBM(D, xc, yc, nxmin, nxmax, nymin, nymax, finout)
 implicit none

 !====================入出力変数=======================================
 real(8), intent(in):: D
 real(8), intent(in):: xc, yc
 integer, intent(in):: nxmin, nxmax, nymin, nymax
 real(8), intent(inout):: finout(1:9, nxmin:nxmax, nymin:nymax)
 !====================================================================
 !====================IBM用定数=======================================
 real(8), parameter:: pi = dacos(-1.d0)	!円周率

 real(8), parameter:: ds = 1.d0		!格子間隔（lattice unit）

 integer, parameter:: itr_max = 5	!反復回数上限（ここでは5回に固定）
 !====================================================================
 !====================IBM用変数=======================================
 integer:: nb				!境界点数
 real(8):: dv				!微小体積

 real(8), allocatable:: xb(:), yb(:)		!境界点座標
 real(8), allocatable:: ub(:), vb(:)		!境界点流速
 real(8), allocatable:: ud(:), vd(:)		!境界で満たすべき流速
 real(8), allocatable:: gbx(:), gby(:)		!境界点体積力

 real(8), allocatable:: us(:,:), vs(:,:)	!一時的な流速
 real(8), allocatable:: gx(:,:), gy(:,:)	!格子点体積力
 real(8), allocatable:: uitr(:,:), vitr(:,:) 	!反復中の流速
 !====================================================================
 !=====================その他定数・変数===============================
 !粒子速度
 real(8), parameter::    cx(9) = (/ 0.d0, 1.d0, 0.d0, -1.d0,  0.d0,  1.d0, -1.d0, -1.d0,  1.d0 /)
 real(8), parameter::    cy(9) = (/ 0.d0, 0.d0, 1.d0,  0.d0, -1.d0,  1.d0,  1.d0, -1.d0, -1.d0 /) 

 !係数
 real(8), parameter::    E(9) = (/ 4.d0/ 9.d0, 1.d0/ 9.d0, 1.d0/ 9.d0, 1.d0/ 9.d0, 1.d0/ 9.d0 ,&
                                1.d0/36.d0, 1.d0/36.d0, 1.d0/36.d0, 1.d0/36.d0 /)

 integer:: i, j, k, l, itr			!インデックス変数
 !====================================================================
 !-----------値の代入------------------
 nb = 4*(int(D)+1)	!境界点数
 dv = pi*D/dble(nb)*ds	!微小体積
 !-------------------------------------
 !======================allocate======================================
 allocate(xb(1:nb))
 allocate(yb(1:nb))
 allocate(ub(1:nb))
 allocate(vb(1:nb))
 allocate(ud(1:nb))
 allocate(vd(1:nb))
 allocate(gbx(1:nb))
 allocate(gby(1:nb))

 allocate(us(nxmin:nxmax, nymin:nymax))
 allocate(vs(nxmin:nxmax, nymin:nymax))
 allocate(gx(nxmin:nxmax, nymin:nymax))
 allocate(gy(nxmin:nxmax, nymin:nymax))
 allocate(uitr(nxmin:nxmax, nymin:nymax))
 allocate(vitr(nxmin:nxmax, nymin:nymax))
 !====================================================================


 !======================境界点の定義==================================
  do k = 1,nb
   xb(k) = xc + 0.5d0*D*dcos(2.d0*pi/dble(nb)*k)
   yb(k) = yc + 0.5d0*D*dsin(2.d0*pi/dble(nb)*k)

   ud(k) = 0.d0
   vd(k) = 0.d0
  end do
 !====================================================================
 !======================一時的な流速の計算============================
  do j = nymin , nymax
   do i = nxmin, nxmax
    us(i,j) =  finout(2,i,j) - finout(4,i,j) + finout(6,i,j) - finout(7,i,j) - finout(8,i,j) + finout(9,i,j)
    vs(i,j) =  finout(3,i,j) - finout(5,i,j) + finout(6,i,j) + finout(7,i,j) - finout(8,i,j) - finout(9,i,j)
   end do
  end do
 !====================================================================
 !======================一時的な流速の内挿============================
  do k = 1, nb

   ub(k)=0.d0
   vb(k)=0.d0

    do j = floor(yb(k)-2.d0), ceiling(yb(k)+2.d0)
     do i = floor(xb(k)-2.d0), ceiling(xb(k)+2.d0)
      ub(k) = ub(k) + us(i,j)*w(i-xb(k))*w(j-yb(k))*ds**2
      vb(k) = vb(k) + vs(i,j)*w(i-xb(k))*w(j-yb(k))*ds**2
     end do
    end do

  end do
 !====================================================================
 !======================Step 0========================================
  do k=1,nb
   gbx(k) = (ud(k)-ub(k))/ds
   gby(k) = (vd(k)-vb(k))/ds
  end do
 !====================================================================
 !======================反復計算======================================
  DO itr = 1,itr_max

   !---------step 1------------------------
   do j = nymin, nymax
    do i = nxmin, nxmax
     gx(i,j)=0.d0
     gy(i,j)=0.d0
    end do
   end do

   do k=1, nb
    do j = floor(yb(k)-2.d0), ceiling(yb(k)+2.d0)
     do i = floor(xb(k)-2.d0), ceiling(xb(k)+2.d0)
      gx(i,j) = gx(i,j) + gbx(k)*w(i-xb(k))*w(j-yb(k))*dv
      gy(i,j) = gy(i,j) + gby(k)*w(i-xb(k))*w(j-yb(k))*dv
     end do
    end do
   end do
   !---------------------------------------
   !---------step 2------------------------
   do j = nymin, nymax
    do i = nxmin, nxmax
      uitr(i,j) = us(i,j) + ds*gx(i,j)
      vitr(i,j) = vs(i,j) + ds*gy(i,j)
    end do
   end do
   !---------------------------------------
   !--------step 3-------------------------
   do k=1,nb

    ub(k)=0.d0
    vb(k)=0.d0

    do j = floor(yb(k)-2.d0), ceiling(yb(k)+2.d0)
     do i = floor(xb(k)-2.d0), ceiling(xb(k)+2.d0)
       ub(k) = ub(k) + uitr(i,j)*w(i-xb(k))*w(j-yb(k))*ds**2
       vb(k) = vb(k) + vitr(i,j)*w(i-xb(k))*w(j-yb(k))*ds**2
     end do
    end do

   end do
   !---------------------------------------
   !--------step 4-------------------------
   do k=1, nb
    gbx(k) = gbx(k) + (ud(k)-ub(k))/ds
    gby(k) = gby(k) + (vd(k)-vb(k))/ds
   end do
   !---------------------------------------
  END DO
 !====================================================================
 !======================体積力の決定==================================
  do j = nymin, nymax
   do i = nxmin, nxmax
    gx(i,j)=0.d0
    gy(i,j)=0.d0
   end do
  end do

  do k=1, nb
   do j = floor(yb(k)-2.d0), ceiling(yb(k)+2.d0)
    do i = floor(xb(k)-2.d0), ceiling(xb(k)+2.d0)
     gx(i,j) = gx(i,j) + gbx(k)*w(i-xb(k))*w(j-yb(k))*dv
     gy(i,j) = gy(i,j) + gby(k)*w(i-xb(k))*w(j-yb(k))*dv
    end do
   end do
  end do
 !====================================================================
 !======================分布関数の更新=================================
  do j = nymin, nymax
   do i = nxmin, nxmax
    
    do l=2,9
     finout(l,i,j) = finout(l,i,j) + 3.d0*ds*E(l)*(cx(l)*gx(i,j)+cy(l)*gy(i,j))
    end do
 
   end do
  end do
 !====================================================================
 !======================deallocate====================================
 deallocate(xb)
 deallocate(yb)
 deallocate(ub)
 deallocate(vb)
 deallocate(ud)
 deallocate(vd)
 deallocate(gbx)
 deallocate(gby)

 deallocate(us)
 deallocate(vs)
 deallocate(gx)
 deallocate(gy)
 deallocate(uitr)
 deallocate(vitr)
 !====================================================================


 contains
 !=================重み関数===========================================
 function w(r)

  real(8)::r, w

  if(dabs(r)<=1.d0) then
   w = 0.125d0*(3.d0-2.d0*dabs(r)+dsqrt(1.d0+4.d0*dabs(r)-4.d0*r**2))

  else if((dabs(r)>1.d0).and.(dabs(r)<=2.d0)) then
   w = 0.125d0*(5.d0-2.d0*dabs(r)-dsqrt(-7.d0+12.d0*dabs(r)-4.d0*r**2))

  else
   w = 0.d0

  end if

 end function
 !====================================================================


 end subroutine circular_IBM