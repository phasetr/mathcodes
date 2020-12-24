program Poiseuille_2D
 implicit none

 !====================LBM用定数=====================================
 real(8), parameter:: ds = 1.d0		!格子間隔（lattice unit）
 real(8), parameter:: rho0 = 1.d0 	!代表密度

 integer, parameter:: imax = 200 	!格子数（x方向）
 integer, parameter:: jmax = 200 	!格子数（y方向）

 real(8), parameter:: Re = 100.d0 	!レイノルズ数

 real(8), parameter:: u0 = 0.03d0	!代表速さ（断面平均流速）
 real(8), parameter:: h0 = dble(jmax)	!代表長さ（壁面間隔）
 real(8), parameter:: nu = u0*h0/Re	!動粘性係数	

 real(8), parameter:: tau = 3.d0*nu/ds + 0.5d0	!緩和時間
 real(8), parameter:: ep = 1.d0/tau 	!tauの逆数

 !圧力差		
 real(8), parameter:: dp = 12.d0*rho0*nu/h0**2*u0 * dble(imax)
				
 !粒子速度
 real(8), parameter::    cx(9) = (/ 0.d0, 1.d0, 0.d0, -1.d0,  0.d0,  1.d0, -1.d0, -1.d0,  1.d0 /)
 real(8), parameter::    cy(9) = (/ 0.d0, 0.d0, 1.d0,  0.d0, -1.d0,  1.d0,  1.d0, -1.d0, -1.d0 /) 

 !係数
 real(8), parameter::    E(9) = (/ 4.d0/ 9.d0, 1.d0/ 9.d0, 1.d0/ 9.d0, 1.d0/ 9.d0, 1.d0/ 9.d0 ,&
                                1.d0/36.d0, 1.d0/36.d0, 1.d0/36.d0, 1.d0/36.d0 /)
 !====================================================================
 !======================LBM用変数=====================================
 real(8), dimension(1:9,0:imax,0:jmax):: f, fnext	!分布関数
 real(8), dimension(0:imax,0:jmax):: p, u, v	 	!圧力，流速
 real(8):: c				!圧力差のある周期条件の定数
 real(8):: u0_eff			!実効断面平均流速
 real(8):: Re_eff			!実効レイノルズ数
 !====================================================================
 !======================円柱用定数====================================
 real(8), parameter:: D = 50.d0*ds		!円柱直径
 real(8), parameter:: xc = 0.5d0*dble(imax)	!円柱の中心位置
 real(8), parameter:: yc = 0.5d0*dble(jmax)

 !円柱の近傍領域
 integer, parameter:: nxmin = floor( xc - 0.5d0*D - 2.d0)
 integer, parameter:: nxmax = ceiling( xc + 0.5d0*D + 2.d0)
 integer, parameter:: nymin = floor( yc - 0.5d0*D - 2.d0)
 integer, parameter:: nymax = ceiling( yc + 0.5d0*D + 2.d0)
 !====================================================================
 !======================円柱用変数====================================
 real(8), dimension(1:9, nxmin:nxmax, nymin:nymax):: finout !入出力用分布関数
 !====================================================================
 !=====================その他定数・変数===============================
 integer, parameter:: step = 500000	!時間ステップ上限

 integer:: i, j, l, n			!インデックス変数
 character*6:: num			!ファイル番号用文字列
 !====================================================================


 !--------------計算条件出力----------------------
  open(8,file='parameter.txt')
  write(8,'(5a20)') 'h0', 'u0', 'Re', 'tau', 'dp'
  write(8,'(5f20.10)') dble(h0), u0, Re, tau, dp
  close(8)
 !------------------------------------------------
 !--------------file open-------------------------
  open(8,file='Re_eff.txt')
 !------------------------------------------------


 !===================初期条件（静止平衡状態）==========================
 do j=0, jmax
  do i=0, imax

   u(i,j) = 0.d0
   v(i,j) = 0.d0
   p(i,j) = rho0/3.d0

   f(1,i,j) = feq1(p(i,j), u(i,j), v(i,j))
   f(2,i,j) = feq2(p(i,j), u(i,j), v(i,j))
   f(3,i,j) = feq3(p(i,j), u(i,j), v(i,j))
   f(4,i,j) = feq4(p(i,j), u(i,j), v(i,j))
   f(5,i,j) = feq5(p(i,j), u(i,j), v(i,j))
   f(6,i,j) = feq6(p(i,j), u(i,j), v(i,j))
   f(7,i,j) = feq7(p(i,j), u(i,j), v(i,j))
   f(8,i,j) = feq8(p(i,j), u(i,j), v(i,j))
   f(9,i,j) = feq9(p(i,j), u(i,j), v(i,j))

  end do
 end do
 !====================================================================


 !=======================時間発展=====================================
 DO n = 1, step


  !-------------分かるものだけ計算（bounce back含む）------
  do j = 0, jmax
   do i = 0, imax

    !領域内部
    if((i/=0).and.(i/=imax).and.(j/=0).and.(j/=jmax)) then
     fnext(1,i,j) = f(1,i,j) - ep*( f(1,i,j) - feq1(p(i,j), u(i,j), v(i,j)) )
     fnext(2,i,j) = f(2,i-1,j) - ep*( f(2,i-1,j) - feq2(p(i-1,j), u(i-1,j), v(i-1,j)) )
     fnext(3,i,j) = f(3,i,j-1) - ep*( f(3,i,j-1) - feq3(p(i,j-1), u(i,j-1), v(i,j-1)) )
     fnext(4,i,j) = f(4,i+1,j) - ep*( f(4,i+1,j) - feq4(p(i+1,j), u(i+1,j), v(i+1,j)) )
     fnext(5,i,j) = f(5,i,j+1) - ep*( f(5,i,j+1) - feq5(p(i,j+1), u(i,j+1), v(i,j+1)) )
     fnext(6,i,j) = f(6,i-1,j-1) - ep*( f(6,i-1,j-1) - feq6(p(i-1,j-1), u(i-1,j-1), v(i-1,j-1)) )
     fnext(7,i,j) = f(7,i+1,j-1) - ep*( f(7,i+1,j-1) - feq7(p(i+1,j-1), u(i+1,j-1), v(i+1,j-1)) )
     fnext(8,i,j) = f(8,i+1,j+1) - ep*( f(8,i+1,j+1) - feq8(p(i+1,j+1), u(i+1,j+1), v(i+1,j+1)) )
     fnext(9,i,j) = f(9,i-1,j+1) - ep*( f(9,i-1,j+1) - feq9(p(i-1,j+1), u(i-1,j+1), v(i-1,j+1)) )


    !下の壁（bounce-back）
    else if((i/=0).and.(i/=imax).and.(j==0)) then
     fnext(1,i,j) = f(1,i,j) - ep*( f(1,i,j) - feq1(p(i,j), u(i,j), v(i,j)) )
     fnext(2,i,j) = f(2,i-1,j) - ep*( f(2,i-1,j) - feq2(p(i-1,j), u(i-1,j), v(i-1,j)) )
     fnext(4,i,j) = f(4,i+1,j) - ep*( f(4,i+1,j) - feq4(p(i+1,j), u(i+1,j), v(i+1,j)) )
     fnext(5,i,j) = f(5,i,j+1) - ep*( f(5,i,j+1) - feq5(p(i,j+1), u(i,j+1), v(i,j+1)) )
     fnext(8,i,j) = f(8,i+1,j+1) - ep*( f(8,i+1,j+1) - feq8(p(i+1,j+1), u(i+1,j+1), v(i+1,j+1)) )
     fnext(9,i,j) = f(9,i-1,j+1) - ep*( f(9,i-1,j+1) - feq9(p(i-1,j+1), u(i-1,j+1), v(i-1,j+1)) )

     fnext(3,i,j) = fnext(5,i,j)
     fnext(6,i,j) = fnext(8,i,j)
     fnext(7,i,j) = fnext(9,i,j)


    !上の壁（bounce-back）
    else if((i/=0).and.(i/=imax).and.(j==jmax)) then
     fnext(1,i,j) = f(1,i,j) - ep*( f(1,i,j) - feq1(p(i,j), u(i,j), v(i,j)) )
     fnext(2,i,j) = f(2,i-1,j) - ep*( f(2,i-1,j) - feq2(p(i-1,j), u(i-1,j), v(i-1,j)) )
     fnext(3,i,j) = f(3,i,j-1) - ep*( f(3,i,j-1) - feq3(p(i,j-1), u(i,j-1), v(i,j-1)) )
     fnext(4,i,j) = f(4,i+1,j) - ep*( f(4,i+1,j) - feq4(p(i+1,j), u(i+1,j), v(i+1,j)) )
     fnext(6,i,j) = f(6,i-1,j-1) - ep*( f(6,i-1,j-1) - feq6(p(i-1,j-1), u(i-1,j-1), v(i-1,j-1)) )
     fnext(7,i,j) = f(7,i+1,j-1) - ep*( f(7,i+1,j-1) - feq7(p(i+1,j-1), u(i+1,j-1), v(i+1,j-1)) )

     fnext(5,i,j) = fnext(3,i,j)
     fnext(8,i,j) = fnext(6,i,j)
     fnext(9,i,j) = fnext(7,i,j)


    !左境界（ここでは分かるものだけ計算，後に周期）
    else if((i==0).and.(j/=0).and.(j/=jmax)) then
     fnext(1,i,j) = f(1,i,j) - ep*( f(1,i,j) - feq1(p(i,j), u(i,j), v(i,j)) )
     fnext(3,i,j) = f(3,i,j-1) - ep*( f(3,i,j-1) - feq3(p(i,j-1), u(i,j-1), v(i,j-1)) )
     fnext(4,i,j) = f(4,i+1,j) - ep*( f(4,i+1,j) - feq4(p(i+1,j), u(i+1,j), v(i+1,j)) )
     fnext(5,i,j) = f(5,i,j+1) - ep*( f(5,i,j+1) - feq5(p(i,j+1), u(i,j+1), v(i,j+1)) )
     fnext(7,i,j) = f(7,i+1,j-1) - ep*( f(7,i+1,j-1) - feq7(p(i+1,j-1), u(i+1,j-1), v(i+1,j-1)) )
     fnext(8,i,j) = f(8,i+1,j+1) - ep*( f(8,i+1,j+1) - feq8(p(i+1,j+1), u(i+1,j+1), v(i+1,j+1)) )


    !右境界（ここでは分かるものだけ計算，後に周期）
    else if((i==imax).and.(j/=0).and.(j/=jmax)) then
     fnext(1,i,j) = f(1,i,j) - ep*( f(1,i,j) - feq1(p(i,j), u(i,j), v(i,j)) )
     fnext(2,i,j) = f(2,i-1,j) - ep*( f(2,i-1,j) - feq2(p(i-1,j), u(i-1,j), v(i-1,j)) )
     fnext(3,i,j) = f(3,i,j-1) - ep*( f(3,i,j-1) - feq3(p(i,j-1), u(i,j-1), v(i,j-1)) )
     fnext(5,i,j) = f(5,i,j+1) - ep*( f(5,i,j+1) - feq5(p(i,j+1), u(i,j+1), v(i,j+1)) )
     fnext(6,i,j) = f(6,i-1,j-1) - ep*( f(6,i-1,j-1) - feq6(p(i-1,j-1), u(i-1,j-1), v(i-1,j-1)) )
     fnext(9,i,j) = f(9,i-1,j+1) - ep*( f(9,i-1,j+1) - feq9(p(i-1,j+1), u(i-1,j+1), v(i-1,j+1)) )


    !左下隅（ここでは分かるものだけ計算，後に周期）
    else if((i==0).and.(j==0)) then
     fnext(1,i,j) = f(1,i,j) - ep*( f(1,i,j) - feq1(p(i,j), u(i,j), v(i,j)) )
     fnext(4,i,j) = f(4,i+1,j) - ep*( f(4,i+1,j) - feq4(p(i+1,j), u(i+1,j), v(i+1,j)) )
     fnext(5,i,j) = f(5,i,j+1) - ep*( f(5,i,j+1) - feq5(p(i,j+1), u(i,j+1), v(i,j+1)) )
     fnext(8,i,j) = f(8,i+1,j+1) - ep*( f(8,i+1,j+1) - feq8(p(i+1,j+1), u(i+1,j+1), v(i+1,j+1)) )

     fnext(3,i,j) = fnext(5,i,j)
     fnext(6,i,j) = fnext(8,i,j)


    !左上隅（ここでは分かるものだけ計算，後に周期）
    else if((i==0).and.(j==jmax)) then
     fnext(1,i,j) = f(1,i,j) - ep*( f(1,i,j) - feq1(p(i,j), u(i,j), v(i,j)) )
     fnext(3,i,j) = f(3,i,j-1) - ep*( f(3,i,j-1) - feq3(p(i,j-1), u(i,j-1), v(i,j-1)) )
     fnext(4,i,j) = f(4,i+1,j) - ep*( f(4,i+1,j) - feq4(p(i+1,j), u(i+1,j), v(i+1,j)) )
     fnext(7,i,j) = f(7,i+1,j-1) - ep*( f(7,i+1,j-1) - feq7(p(i+1,j-1), u(i+1,j-1), v(i+1,j-1)) )

     fnext(5,i,j) = fnext(3,i,j)
     fnext(9,i,j) = fnext(7,i,j)


    !右下隅（ここでは分かるものだけ計算，後に周期）
    else if((i==imax).and.(j==0)) then
     fnext(1,i,j) = f(1,i,j) - ep*( f(1,i,j) - feq1(p(i,j), u(i,j), v(i,j)) )
     fnext(2,i,j) = f(2,i-1,j) - ep*( f(2,i-1,j) - feq2(p(i-1,j), u(i-1,j), v(i-1,j)) )
     fnext(5,i,j) = f(5,i,j+1) - ep*( f(5,i,j+1) - feq5(p(i,j+1), u(i,j+1), v(i,j+1)) )
     fnext(9,i,j) = f(9,i-1,j+1) - ep*( f(9,i-1,j+1) - feq9(p(i-1,j+1), u(i-1,j+1), v(i-1,j+1)) )

     fnext(3,i,j) = fnext(5,i,j)
     fnext(7,i,j) = fnext(9,i,j)


    !右上隅（ここでは分かるものだけ計算，後に周期）
    else if((i==imax).and.(j==jmax)) then
     fnext(1,i,j) = f(1,i,j) - ep*( f(1,i,j) - feq1(p(i,j), u(i,j), v(i,j)) )
     fnext(2,i,j) = f(2,i-1,j) - ep*( f(2,i-1,j) - feq2(p(i-1,j), u(i-1,j), v(i-1,j)) )
     fnext(3,i,j) = f(3,i,j-1) - ep*( f(3,i,j-1) - feq3(p(i,j-1), u(i,j-1), v(i,j-1)) )
     fnext(6,i,j) = f(6,i-1,j-1) - ep*( f(6,i-1,j-1) - feq6(p(i-1,j-1), u(i-1,j-1), v(i-1,j-1)) )

     fnext(5,i,j) = fnext(3,i,j)
     fnext(8,i,j) = fnext(6,i,j)

    end if

   end do
  end do
  !---------------------------------------------------------
  !-------------圧力差のある周期条件------------------------
  do j = 0, jmax
   c = dp - (fnext(1,0,j)-fnext(1,imax,j) + fnext(3,0,j)-fnext(3,imax,j) + fnext(5,0,j)-fnext(5,imax,j))/3.d0

   if((j/=0).and.(j/=jmax)) then
    fnext(2,0,j) = fnext(2,imax,j) + c
    fnext(6,0,j) = fnext(6,imax,j) + 0.25d0*c
    fnext(9,0,j) = fnext(9,imax,j) + 0.25d0*c
     
    fnext(4,imax,j) = fnext(4,0,j) - c
    fnext(7,imax,j) = fnext(7,0,j) - 0.25d0*c
    fnext(8,imax,j) = fnext(8,0,j) - 0.25d0*c

   else if(j == 0) then
    fnext(2,0,j) = fnext(2,imax,j) + c
    fnext(9,0,j) = fnext(9,imax,j) + 0.25d0*c

    fnext(4,imax,j) = fnext(4,0,j) - c
    fnext(8,imax,j) = fnext(8,0,j) - 0.25d0*c

   else if(j == jmax) then
    fnext(2,0,j) = fnext(2,imax,j) + c
    fnext(6,0,j) = fnext(6,imax,j) + 0.25d0*c

    fnext(4,imax,j) = fnext(4,0,j) - c
    fnext(7,imax,j) = fnext(7,0,j) - 0.25d0*c

   end if

  end do
  !---------------------------------------------------------
  !---------------角の境界条件（残り）----------------------
  i = 0; j = 0
  fnext(7,i,j) = fnext(9,i,j)

  i = imax; j = 0
  fnext(6,i,j) = fnext(8,i,j)

  i = 0; j = jmax
  fnext(8,i,j) = fnext(6,i,j)

  i = imax; j = jmax
  fnext(9,i,j) = fnext(7,i,j)
  !---------------------------------------------------------
  !-------------サブルーチン入力用変数の準備----------------
  do j = nymin, nymax
   do i = nxmin, nxmax
    do l=1,9
     finout(l,i,j) = fnext(l,i,j)
    end do
   end do
  end do
  !---------------------------------------------------------
  !-------------円柱の境界条件------------------------------
  !円柱がない場合：両方ともコメントアウト
  !円柱の境界条件を改良bounce-backで計算する場合：circular_IBB
  !円柱の境界条件を埋め込み境界法で計算する場合：circular_IBM

  !CALL circular_IBB(D, xc, yc, nxmin, nxmax, nymin, nymax, finout)
  !CALL circular_IBM(D, xc, yc, nxmin, nxmax, nymin, nymax, finout)
  !---------------------------------------------------------
  !-------------更新----------------------------------------
  do j = nymin, nymax
   do i = nxmin, nxmax
    do l=1,9
     fnext(l,i,j) = finout(l,i,j)
    end do
   end do
  end do


  do j = 0, jmax
   do i = 0, imax

    do l=1,9
     f(l,i,j) = fnext(l,i,j)
    end do

    p(i,j) = (f(1,i,j) + f(2,i,j) + f(3,i,j) + f(4,i,j) + f(5,i,j) + f(6,i,j) + f(7,i,j) + f(8,i,j) + f(9,i,j))/3.d0
    u(i,j) =  f(2,i,j) - f(4,i,j) + f(6,i,j) - f(7,i,j) - f(8,i,j) + f(9,i,j)
    v(i,j) =  f(3,i,j) - f(5,i,j) + f(6,i,j) + f(7,i,j) - f(8,i,j) - f(9,i,j)

   end do
  end do
  !---------------------------------------------------------

  !---------------出力--------------------------------------
  !----------実効レイノルズ数----------------
  u0_eff = 0.d0
  do j=0,jmax-1
   u0_eff = u0_eff + 0.5d0*(u(0,j) + u(0,j+1))*ds
  end do
  u0_eff = u0_eff/h0
  Re_eff = u0_eff*h0/nu

  write(8,'(3f20.15)') n/(h0/u0), u0_eff/u0, Re_eff
  !------------------------------------------
  !-----------流速・圧力分布（gnuplot用）----
  if(mod(n, step/100) == 0) then

    num='      '
    write(num,'(1i6)') n
    do j=1,6
     if(num(j:j)==' ') num(j:j)='0'
    end do

    open(11,file='flow'//num//'.txt')

    do j=0, jmax
     do i=0, imax
      write(11,'(2f20.15, 1x, 3f20.15)') i*ds/h0, j*ds/h0, u(i,j)/u0, v(i,j)/u0, (p(i,j)-rho0/3.d0)/(rho0*u0**2)
     end do
      write(11,*)
    end do

    close(11)
  end if
  !------------------------------------------
  !----------------------------------------------------------


 END DO
 !====================================================================

 close(8)


 contains
 !====================================================================平衡分布関数の定義
 !--------------------------------------
 function feq1(pp, up, vp)
  real(8):: feq1, pp, up, vp
  
  feq1 = E(1)*(3.d0*pp + 3.d0*(up*cx(1) + vp*cy(1)) + 4.5d0*(up*cx(1) + vp*cy(1))**2 -1.5d0*(up**2 + vp**2) )

 end function
 !--------------------------------------
 !--------------------------------------
 function feq2(pp, up, vp)
  real(8):: feq2, pp, up, vp
  
  feq2 = E(2)*(3.d0*pp + 3.d0*(up*cx(2) + vp*cy(2)) + 4.5d0*(up*cx(2) + vp*cy(2))**2 -1.5d0*(up**2 + vp**2) )

 end function
 !--------------------------------------
 !--------------------------------------
 function feq3(pp, up, vp)
  real(8):: feq3, pp, up, vp
  
  feq3 = E(3)*(3.d0*pp + 3.d0*(up*cx(3) + vp*cy(3)) + 4.5d0*(up*cx(3) + vp*cy(3))**2 -1.5d0*(up**2 + vp**2) )

 end function
 !--------------------------------------
 !--------------------------------------
 function feq4(pp, up, vp)
  real(8):: feq4, pp, up, vp
  
  feq4 = E(4)*(3.d0*pp + 3.d0*(up*cx(4) + vp*cy(4)) + 4.5d0*(up*cx(4) + vp*cy(4))**2 -1.5d0*(up**2 + vp**2) )

 end function
 !--------------------------------------
 !--------------------------------------
 function feq5(pp, up, vp)
  real(8):: feq5, pp, up, vp
  
  feq5 = E(5)*(3.d0*pp + 3.d0*(up*cx(5) + vp*cy(5)) + 4.5d0*(up*cx(5) + vp*cy(5))**2 -1.5d0*(up**2 + vp**2) )

 end function
 !--------------------------------------
 !--------------------------------------
 function feq6(pp, up, vp)
  real(8):: feq6, pp, up, vp
  
  feq6 = E(6)*(3.d0*pp + 3.d0*(up*cx(6) + vp*cy(6)) + 4.5d0*(up*cx(6) + vp*cy(6))**2 -1.5d0*(up**2 + vp**2) )

 end function
 !--------------------------------------
 !--------------------------------------
 function feq7(pp, up, vp)
  real(8):: feq7, pp, up, vp
  
  feq7 = E(7)*(3.d0*pp + 3.d0*(up*cx(7) + vp*cy(7)) + 4.5d0*(up*cx(7) + vp*cy(7))**2 -1.5d0*(up**2 + vp**2) )

 end function
 !--------------------------------------
 !--------------------------------------
 function feq8(pp, up, vp)
  real(8):: feq8, pp, up, vp
  
  feq8 = E(8)*(3.d0*pp + 3.d0*(up*cx(8) + vp*cy(8)) + 4.5d0*(up*cx(8) + vp*cy(8))**2 -1.5d0*(up**2 + vp**2) )

 end function
 !--------------------------------------
 !--------------------------------------
 function feq9(pp, up, vp)
  real(8):: feq9, pp, up, vp
  
  feq9 = E(9)*(3.d0*pp + 3.d0*(up*cx(9) + vp*cy(9)) + 4.5d0*(up*cx(9) + vp*cy(9))**2 -1.5d0*(up**2 + vp**2) )

 end function
 !--------------------------------------

end program