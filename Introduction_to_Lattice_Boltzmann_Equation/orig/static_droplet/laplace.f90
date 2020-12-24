program laplace
 implicit none

 !====================LBM用定数=====================================
 real(8), parameter:: ds = 1.d0		!格子間隔（lattice unit）

 integer, parameter:: imax = 96 	!格子数（x方向）
 integer, parameter:: jmax = 96 	!格子数（y方向）
 integer, parameter:: kmax = 96 	!格子数（z方向）

 real(8), parameter:: D = 40.d0*ds	!初期液滴直径

 real(8), parameter:: h0 = D		!代表長さ

 !初期液滴中心
 real(8), parameter:: xc = 0.5d0*dble(imax)
 real(8), parameter:: yc = 0.5d0*dble(jmax)
 real(8), parameter:: zc = 0.5d0*dble(kmax)

 !粒子速度（整数）
 integer, parameter:: ci(15) = (/ 0, 1, 0,  0, -1,  0,  0,  1, -1,  1,  1, -1,  1, -1, -1/)
 integer, parameter:: cj(15) = (/ 0, 0, 1,  0,  0, -1,  0,  1,  1, -1,  1, -1, -1,  1, -1/)
 integer, parameter:: ck(15) = (/ 0, 0, 0,  1,  0,  0, -1,  1,  1,  1, -1, -1, -1, -1,  1/)

 !粒子速度（実数）
 real(8):: cr(1:3, 1:15)

 !係数
 real(8), parameter:: E(15) = (/ 2.d0/9.d0, 1.d0/9.d0, 1.d0/9.d0, 1.d0/9.d0, 1.d0/9.d0, 1.d0/9.d0, 1.d0/9.d0, &
                                1.d0/72.d0, 1.d0/72.d0, 1.d0/72.d0, 1.d0/72.d0, &
                                1.d0/72.d0, 1.d0/72.d0, 1.d0/72.d0, 1.d0/72.d0 /)
 real(8), parameter:: H(15) = (/ 1.d0, 0.d0, 0.d0, 0.d0, 0.d0, 0.d0, 0.d0, 0.d0, &
                                 0.d0, 0.d0, 0.d0, 0.d0, 0.d0, 0.d0, 0.d0 /)
 real(8), parameter:: F(15) = (/ -7.d0/3.d0, 1.d0/3.d0, 1.d0/3.d0, 1.d0/3.d0, 1.d0/3.d0, 1.d0/3.d0, 1.d0/3.d0, &
                                1.d0/24.d0, 1.d0/24.d0, 1.d0/24.d0, 1.d0/24.d0, &
                                1.d0/24.d0, 1.d0/24.d0, 1.d0/24.d0, 1.d0/24.d0 /)
 !====================================================================
 !====================支配パラメタ====================================
 real(8), parameter:: ratio_rho	= 800.d0	!密度比
 real(8), parameter:: ratio_mu = 50.d0		!粘度比

 real(8), parameter:: rhog = 1.d0		!気相密度
 real(8), parameter:: rhol = ratio_rho*rhog	!液相密度

 real(8), parameter:: mug = 1.6d-2*ds		!気相粘度
 real(8), parameter:: mul = ratio_mu*mug	!液相粘度

 real(8), parameter:: sigma  = 1.2d-2*ds	!界面張力
 !====================================================================
 !=====================phi計算用定数==================================
 real(8), parameter:: a = 1.d0
 real(8), parameter:: b = 1.d0
 real(8), parameter:: T = 2.93d-1
 real(8), parameter:: phi1 = 2.638d-1
 real(8), parameter:: phi2 = 4.031d-1

 real(8), parameter:: kappaf = 0.06d0*ds**2
 real(8), parameter:: C = 0.d0
 real(8), parameter:: M = (0.5d0-C/3.d0)*ds		!モビリティ
 !====================================================================
 !======================phi計算用変数=================================
 real(8), dimension(1:15,0:imax,0:jmax,0:kmax):: feq		!平衡分布関数

 real(8), dimension(0:imax,0:jmax,0:kmax):: phi		!index function

 !index function計算用中間変数
 real(8), dimension(0:imax,0:jmax,0:kmax):: lap_phi
 real(8), dimension(1:3, 0:imax,0:jmax,0:kmax):: grad_phi
 real(8), dimension(0:imax,0:jmax,0:kmax):: p0
 real(8), dimension(1:3, 1:3, 0:imax,0:jmax,0:kmax):: gphi
 real(8), dimension(1:3, 1:3, 0:imax,0:jmax,0:kmax):: pcap
 real(8), dimension(1:3, 0:imax,0:jmax,0:kmax):: div_pcap

 real(8):: phi_min, phi_max			!phiの最小値，最大値
 !====================================================================
 !=====================圧力・流速計算用定数===========================
 integer, parameter:: itr_max = 2			!反復回数
 real(8), parameter:: omega_max = rhol/dble(itr_max)	!加速パラメタの最大値
 real(8), parameter:: lambda = 1.d0			!安定項の係数
 !====================================================================
 !======================圧力・流速計算用変数==========================
 real(8), dimension(1:15,0:imax,0:jmax,0:kmax):: geq		!平衡分布関数

 real(8), dimension(0:imax,0:jmax,0:kmax):: rho			!密度
 real(8), dimension(0:imax,0:jmax,0:kmax):: p			!圧力
 real(8), dimension(0:imax,0:jmax,0:kmax):: u, v, w		!流速

 real(8), dimension(0:imax,0:jmax,0:kmax):: omega		!加速パラメタ
 real(8), dimension(1:15,0:imax,0:jmax,0:kmax):: delta_p	!圧力計算時の中間変数

 real(8), dimension(0:imax,0:jmax,0:kmax):: rhonext		!次ステップの密度
 real(8), dimension(0:imax,0:jmax,0:kmax):: unext, vnext, wnext	!次ステップの流速

 real(8), dimension(0:imax,0:jmax,0:kmax):: mu			!粘度
 real(8), dimension(0:imax,0:jmax,0:kmax)::Au			!粘度に関する変数

 real(8), dimension(1:3, 0:imax,0:jmax,0:kmax)::grad_mu		!粘度の勾配
 real(8), dimension(1:3,1:3, 0:imax,0:jmax,0:kmax)::grad_u	!流速勾配
 real(8), dimension(1:3,0:imax,0:jmax,0:kmax):: vcap		!V_alpha

 real(8), dimension(0:imax,0:jmax,0:kmax)::lap_u		!uのラプラシアン
 real(8), dimension(0:imax,0:jmax,0:kmax)::lap_v		!vのラプラシアン
 real(8), dimension(0:imax,0:jmax,0:kmax)::lap_w		!wのラプラシアン
 real(8), dimension(0:imax,0:jmax,0:kmax)::laplap_u		!lap_uのラプラシアン
 real(8), dimension(0:imax,0:jmax,0:kmax)::laplap_v		!lap_vのラプラシアン
 real(8), dimension(0:imax,0:jmax,0:kmax)::laplap_w		!lap_wのラプラシアン

 real(8), dimension(1:3, 0:imax,0:jmax,0:kmax)::grad_rho	!密度の勾配
 real(8), dimension(1:3, 0:imax,0:jmax,0:kmax)::normal		!法線方向
 real(8), dimension(0:imax,0:jmax,0:kmax)::chi			!曲率
 real(8), dimension(1:3, 0:imax,0:jmax,0:kmax)::fsv		!界面張力
 !====================================================================
 !=====================その他定数・変数===============================
 integer, parameter:: step = 20000		!時間ステップ上限

 integer, parameter:: start_sigma = 5000	!界面張力をかけ始めるステップ数
 integer, parameter:: end_sigma = 15000		!界面張力をかけ終わるステップ数
 real(8):: sigma_temp				!各ステップにおける界面張力の値

 real(8):: epsilon = 1.d-12			!零除算を防ぐための閾値

 real(8):: krone(1:3,1:3)			!クロネッカーのデルタ

 real(8):: gtemp				!一時的な変数

 real(8), parameter:: pi = dacos(-1.d0)		!円周率

 real(8), parameter:: rinput = 0.5d0*D			!インプット半径
 real(8), parameter:: vinput = 4.d0/3.d0*pi*rinput**3	!インプット体積

 real(8):: veff, reff				!実効液滴体積，半径
 real(8):: dp_th 				!Laplace圧の理論値
 real(8):: pg, pl, dp_calc			!ガス圧，液圧，気液圧力差の計算値
 real(8):: err					!Laplace圧からの誤差

 integer:: i, j, k, l, n, alpha, beta, itr	!インデックス変数
 character*6:: num				!ファイル番号用文字列
 !====================================================================
 !-----------値の代入-----------------------------
 do l=1,15
  cr(1,l) = dble(ci(l))
  cr(2,l) = dble(cj(l))
  cr(3,l) = dble(ck(l))
 end do

 krone(1,1) = 1.d0; krone(1,2) = 0.d0; krone(1,3) = 0.d0
 krone(2,1) = 0.d0; krone(2,2) = 1.d0; krone(2,3) = 0.d0
 krone(3,1) = 0.d0; krone(3,2) = 0.d0; krone(3,3) = 1.d0
 !------------------------------------------------
 !--------------計算条件出力----------------------
  open(8,file='parameter.txt')
  write(8,'(11a20)') 'h0','ratio_rho','ratio_mu','mug','mul','sigma','kappaf','C','M','itr_max','omega_max'
  write(8,'(11f20.10)') h0, ratio_rho, ratio_mu, mug, mul, sigma, kappaf, C, M, dble(itr_max), omega_max
  close(8)
 !------------------------------------------------
 !--------------file open-------------------------
  open(8,file='dp.txt')
  open(9,file='reff.txt')
 !------------------------------------------------


 !======================初期条件======================================
 do k=0, kmax
  do j=0,jmax
   do i=0,imax

    u(i,j,k) = 0.d0
    v(i,j,k) = 0.d0
    w(i,j,k) = 0.d0
    p(i,j,k) = 1.d0/3.d0
    phi(i,j,k) = phi1

    if((i*ds-xc)**2+(j*ds-yc)**2+(k*ds-zc)**2 <= (0.5d0*D)**2) then
     phi(i,j,k) = phi2
    end if

    !密度の関数形：線形補間，三角関数のどちらかを選択
    !rho(i,j,k) = (phi(i,j,k)-phi1)/(phi2-phi1)*(rhol-rhog) + rhog
    rho(i,j,k) = 0.5d0*(rhol-rhog)*(dsin(pi*(phi(i,j,k)-0.5d0*(phi2+phi1))/(phi2-phi1)) + 1.d0) + rhog

   end do
  end do
 end do
 !====================================================================


 !======================時間発展======================================
 DO n=1,step
 !----------------------------------------------------------phiの計算
 !-----------lap_phiの計算（全面周期条件）-------
 do k=0,kmax
  do j=0,jmax
   do i=0,imax
    lap_phi(i,j,k) = - 14.d0*phi(i,j,k)

    !bulk部分
    if((i/=0).and.(i/=imax).and.(j/=0).and.(j/=jmax).and.(k/=0).and.(k/=kmax)) then
     do l=2,15
      lap_phi(i,j,k) = lap_phi(i,j,k) + phi(i+ci(l),j+cj(l),k+ck(l))
     end do

    !境界（周期条件）
    else
     do l=2,15
      lap_phi(i,j,k) = lap_phi(i,j,k) + phi(perx(i+ci(l)),pery(j+cj(l)),perz(k+ck(l)))
     end do
    end if

    lap_phi(i,j,k) = lap_phi(i,j,k) / (5.d0*ds**2)

   end do
  end do
 end do
 !-----------------------------------------------
 !-----------grad_phiの計算（全面周期）----------
 do k=0,kmax
  do j=0,jmax
   do i=0,imax

    do alpha=1,3
     grad_phi(alpha,i,j,k) = 0.d0

     !bulk部分
     if((i/=0).and.(i/=imax).and.(j/=0).and.(j/=jmax).and.(k/=0).and.(k/=kmax)) then
      do l=2,15
       grad_phi(alpha,i,j,k) = grad_phi(alpha,i,j,k) + cr(alpha,l)*phi(i+ci(l),j+cj(l),k+ck(l))
      end do

     !境界（周期条件）
     else
      do l=2,15
       grad_phi(alpha,i,j,k) = grad_phi(alpha,i,j,k) + cr(alpha,l)*phi(perx(i+ci(l)),pery(j+cj(l)),perz(k+ck(l)))
      end do
     end if

     grad_phi(alpha,i,j,k) = grad_phi(alpha,i,j,k) / (10.d0*ds)

    end do

   end do
  end do
 end do
 !-----------------------------------------------
 !-----------p0の計算----------------------------
 do k=0,kmax
  do j=0,jmax
   do i=0,imax
    p0(i,j,k) = phi(i,j,k)*T/(1.d0-b*phi(i,j,k)) - a*phi(i,j,k)**2
   end do
  end do
 end do
 !-----------------------------------------------
 !-----------gphiの計算--------------------------
 do k=0,kmax
  do j=0,jmax
   do i=0,imax

    do beta=1,3
     do alpha=1,3
      gphi(alpha,beta,i,j,k) = 4.5d0*grad_phi(alpha,i,j,k)*grad_phi(beta,i,j,k) &
                      - 1.5d0*(grad_phi(1,i,j,k)**2 + grad_phi(2,i,j,k)**2 + grad_phi(3,i,j,k)**2)&
                             *krone(alpha,beta)
     end do
    end do

   end do
  end do
 end do
 !-----------------------------------------------
 !-----------pcapの計算--------------------------
 do k=0,kmax
  do j=0,jmax
   do i=0,imax

    do beta=1,3
     do alpha=1,3
      pcap(alpha,beta,i,j,k) = (p0(i,j,k)-kappaf*phi(i,j,k)*lap_phi(i,j,k) &
                                -0.5d0*kappaf*(grad_phi(1,i,j,k)**2 &
                                             + grad_phi(2,i,j,k)**2 &
                                             + grad_phi(3,i,j,k)**2))*krone(alpha,beta) &
                                + kappaf*grad_phi(alpha,i,j,k)*grad_phi(beta,i,j,k)
     end do
    end do

   end do
  end do
 end do
 !-----------------------------------------------
 !------div_pcapの計算（全面周期条件）-----------
 do k=0,kmax
  do j=0,jmax
   do i=0,imax

    do alpha=1,3
     div_pcap(alpha,i,j,k) = 0.d0

     !bulk部分
     if((i/=0).and.(i/=imax).and.(j/=0).and.(j/=jmax).and.(k/=0).and.(k/=kmax)) then
      do l=2,15
       do beta = 1,3
        div_pcap(alpha,i,j,k) = div_pcap(alpha,i,j,k) + cr(beta,l)*pcap(alpha,beta,i+ci(l),j+cj(l),k+ck(l))
       end do
      end do

     !境界（周期条件）
     else
      do l=2,15
       do beta = 1,3
        div_pcap(alpha,i,j,k) = div_pcap(alpha,i,j,k) &
                              + cr(beta,l)*pcap(alpha,beta,perx(i+ci(l)),pery(j+cj(l)),perz(k+ck(l)))
       end do
      end do
     end if

     div_pcap(alpha,i,j,k) = div_pcap(alpha,i,j,k) / (10.d0*ds)

    end do

   end do
  end do
 end do
 !-----------------------------------------------
 !-----------feqの計算---------------------------
 do k=0,kmax
  do j=0,jmax
   do i=0,imax

    do l=1,15

     gtemp = 0.d0

     do beta=1,3
      do alpha=1,3
       gtemp = gtemp + gphi(alpha,beta,i,j,k)*cr(alpha,l)*cr(beta,l)
      end do
     end do

     feq(l,i,j,k) = H(l)*phi(i,j,k) &
                  + F(l)*(p0(i,j,k)-kappaf*phi(i,j,k)*lap_phi(i,j,k)&
                          -kappaf/6.d0*(grad_phi(1,i,j,k)**2 + grad_phi(2,i,j,k)**2 + grad_phi(3,i,j,k)**2)) &
                  + 3.d0*E(l)*phi(i,j,k)*(cr(1,l)*u(i,j,k)+cr(2,l)*v(i,j,k)+cr(3,l)*w(i,j,k)) &
                  + E(l)*kappaf*gtemp &
                  + E(l)*C*(div_pcap(1,i,j,k)*cr(1,l)+div_pcap(2,i,j,k)*cr(2,l)+div_pcap(3,i,j,k)*cr(3,l))*ds
    end do

   end do
  end do
 end do
 !-----------------------------------------------
 !***********phiの時間発展（全面周期）***********
 do k=0,kmax
  do j=0,jmax
   do i=0,imax

    phi(i,j,k) = 0.d0

    !bulk部分
    if((i/=0).and.(i/=imax).and.(j/=0).and.(j/=jmax).and.(k/=0).and.(k/=kmax)) then
     do l=1,15
      phi(i,j,k) = phi(i,j,k) + feq(l,i-ci(l), j-cj(l), k-ck(l))
     end do

    !境界（周期条件）
    else
     do l=1,15
      phi(i,j,k) = phi(i,j,k) + feq(l,perx(i-ci(l)), pery(j-cj(l)), perz(k-ck(l)))
     end do
    end if

   end do
  end do
 end do
 !************************************************
 !----------------------------------------------------------
 !----------------------------------------------------------pの計算
 !--------------omegaの計算-----------------------
 do k=0,kmax
  do j=0,jmax
   do i=0,imax
    omega(i,j,k) = 1.d0 + (rho(i,j,k)-rhog)/(rhol-rhog)*(omega_max-1.d0)
   end do
  end do
 end do
 !------------------------------------------------
 !--------geqの計算-------------------------------
 do k=0,kmax
  do j=0,jmax
   do i=0,imax
    do l=1,15
     geq(l,i,j,k) = E(l)*(3.d0*(cr(1,l)*u(i,j,k)+cr(2,l)*v(i,j,k)+cr(3,l)*w(i,j,k)) &
                        - 1.5d0*(u(i,j,k)**2+v(i,j,k)**2+w(i,j,k)**2) &
                        + 4.5d0*(cr(1,l)*u(i,j,k)+cr(2,l)*v(i,j,k)+cr(3,l)*w(i,j,k))**2)
    end do
   end do
  end do
 end do
 !------------------------------------------------
 !********反復計算********************************
 do itr = 1, itr_max

  !--------delta_pの計算（全面周期）----
  do k=0,kmax
   do j=0,jmax
    do i=0,imax

     !bulk部分
     if((i/=0).and.(i/=imax).and.(j/=0).and.(j/=jmax).and.(k/=0).and.(k/=kmax)) then
      do l=1,15
       delta_p(l,i,j,k) = 1.5d0*E(l)*(1.d0/rho(i-ci(l),j-cj(l),k-ck(l)) + 1.d0/rho(i,j,k)) &
                                    * (p(i-ci(l),j-cj(l),k-ck(l)) - p(i,j,k))
      end do

     !境界（周期条件）
     else
      do l=1,15
       delta_p(l,i,j,k) = 1.5d0*E(l)*(1.d0/rho(perx(i-ci(l)),pery(j-cj(l)),perz(k-ck(l))) + 1.d0/rho(i,j,k)) &
                                    *(p(perx(i-ci(l)),pery(j-cj(l)),perz(k-ck(l))) - p(i,j,k))
      end do
     end if

    end do
   end do
  end do
  !-------------------------------------
  !--------pの発展計算（全面周期）------
  do k=0,kmax
   do j=0,jmax
    do i=0,imax

     !bulk部分
     if((i/=0).and.(i/=imax).and.(j/=0).and.(j/=jmax).and.(k/=0).and.(k/=kmax)) then
      do l=1,15
       p(i,j,k) = p(i,j,k) + omega(i,j,k)/3.d0*(delta_p(l,i,j,k) + geq(l,i-ci(l), j-cj(l), k-ck(l)))
      end do

     !境界（周期条件）
     else
      do l=1,15
       p(i,j,k) = p(i,j,k) &
                 + omega(i,j,k)/3.d0*(delta_p(l,i,j,k) + geq(l,perx(i-ci(l)), pery(j-cj(l)), perz(k-ck(l))))
      end do
     end if

    end do
   end do
  end do
  !-------------------------------------

 end do
 !************************************************
 !----------------------------------------------------------
 !----------------------------------------------------------流速の計算
 !---------次ステップのrho------------------------
 phi_min = phi2
 phi_max = phi1

 do k=0,kmax
  do j=0,jmax
   do i=0,imax
    if(phi_min > phi(i,j,k)) phi_min = phi(i,j,k)
    if(phi_max < phi(i,j,k)) phi_max = phi(i,j,k)
   end do
  end do
 end do


 do k=0,kmax
  do j=0,jmax
   do i=0,imax

    !密度の関数形：線形補間，三角関数のどちらかを選択
    !rhonext(i,j,k) = (phi(i,j,k)-phi_min)/(phi_max-phi_min) + rhog
    rhonext(i,j,k) = 0.5d0*(rhol-rhog)*(dsin(pi*(phi(i,j,k)-0.5d0*(phi_max+phi_min))/(phi_max-phi_min)) + 1.d0)&
                   + rhog

   end do
  end do
 end do
 !-----------------------------------------------
 !--------delta_pの計算（全面周期）--------------
  do k=0,kmax
   do j=0,jmax
    do i=0,imax

     !bulk部分
     if((i/=0).and.(i/=imax).and.(j/=0).and.(j/=jmax).and.(k/=0).and.(k/=kmax)) then
      do l=1,15
       delta_p(l,i,j,k) = 1.5d0*E(l)*(1.d0/rhonext(i-ci(l),j-cj(l),k-ck(l)) + 1.d0/rhonext(i,j,k)) &
                                    *(p(i-ci(l),j-cj(l),k-ck(l)) - p(i,j,k))
      end do

     !境界（周期条件）
     else
      do l=1,15
       delta_p(l,i,j,k) = 1.5d0*E(l)*(1.d0/rhonext(perx(i-ci(l)),pery(j-cj(l)),perz(k-ck(l))) &
                                    + 1.d0/rhonext(i,j,k)) &
                                    *(p(perx(i-ci(l)),pery(j-cj(l)),perz(k-ck(l))) - p(i,j,k))
      end do
     end if

    end do
   end do
  end do
 !------------------------------------------------
 !---------muの計算（線形補間）-------------------
 do k=0,kmax
  do j=0,jmax
   do i=0,imax
    mu(i,j,k) = mug + (rho(i,j,k)-rhog)/(rhol-rhog)*(mul-mug)
   end do
  end do
 end do
 !------------------------------------------------
 !---------Auの計算-------------------------------
 do k=0,kmax
  do j=0,jmax
   do i=0,imax
    Au(i,j,k) = 1.d0 - 6.d0*mu(i,j,k)/rho(i,j,k)/ds
   end do
  end do
 end do
 !------------------------------------------------
 !---------grad_muの計算（全面周期）--------------
 do k=0,kmax
  do j=0,jmax
   do i=0,imax

    do alpha=1,3
     grad_mu(alpha,i,j,k) = 0.d0

     !bulk部分
     if((i/=0).and.(i/=imax).and.(j/=0).and.(j/=jmax).and.(k/=0).and.(k/=kmax)) then
      do l=2,15
       grad_mu(alpha,i,j,k) = grad_mu(alpha,i,j,k) + cr(alpha,l)*mu(i+ci(l),j+cj(l),k+ck(l))
      end do

     !境界（周期条件）
     else
      do l=2,15
       grad_mu(alpha,i,j,k) = grad_mu(alpha,i,j,k) + cr(alpha,l)*mu(perx(i+ci(l)),pery(j+cj(l)),perz(k+ck(l)))
      end do
     end if

     grad_mu(alpha,i,j,k) = grad_mu(alpha,i,j,k) / (10.d0*ds)

    end do

   end do
  end do
 end do
 !-----------------------------------------------
 !--------grad_uの計算（全面周期）---------------
 do k=0,kmax
  do j=0,jmax
   do i=0,imax

    do beta=1,3
     grad_u(1,beta,i,j,k) = 0.d0
     grad_u(2,beta,i,j,k) = 0.d0
     grad_u(3,beta,i,j,k) = 0.d0

     !bulk部分
     if((i/=0).and.(i/=imax).and.(j/=0).and.(j/=jmax).and.(k/=0).and.(k/=kmax)) then
      do l=2,15
       grad_u(1,beta,i,j,k) = grad_u(1,beta,i,j,k) + cr(beta,l)*u(i+ci(l),j+cj(l),k+ck(l))
       grad_u(2,beta,i,j,k) = grad_u(2,beta,i,j,k) + cr(beta,l)*v(i+ci(l),j+cj(l),k+ck(l))
       grad_u(3,beta,i,j,k) = grad_u(3,beta,i,j,k) + cr(beta,l)*w(i+ci(l),j+cj(l),k+ck(l))
      end do

     !境界（周期条件）
     else
      do l=2,15
       grad_u(1,beta,i,j,k) = grad_u(1,beta,i,j,k) + cr(beta,l)*u(perx(i+ci(l)),pery(j+cj(l)),perz(k+ck(l)))
       grad_u(2,beta,i,j,k) = grad_u(2,beta,i,j,k) + cr(beta,l)*v(perx(i+ci(l)),pery(j+cj(l)),perz(k+ck(l)))
       grad_u(3,beta,i,j,k) = grad_u(3,beta,i,j,k) + cr(beta,l)*w(perx(i+ci(l)),pery(j+cj(l)),perz(k+ck(l)))
      end do
     end if

     grad_u(1,beta,i,j,k) = grad_u(1,beta,i,j,k) / (10.d0*ds)
     grad_u(2,beta,i,j,k) = grad_u(2,beta,i,j,k) / (10.d0*ds)
     grad_u(3,beta,i,j,k) = grad_u(3,beta,i,j,k) / (10.d0*ds)

    end do

   end do
  end do
 end do
 !------------------------------------------------
 !--------vcapの計算------------------------------
 do k=0,kmax
  do j=0,jmax
   do i=0,imax

    do alpha=1,3
     vcap(alpha,i,j,k) = 0.d0

     do beta = 1,3
      vcap(alpha,i,j,k) = vcap(alpha,i,j,k) &
                         + 1.d0/rho(i,j,k)*grad_mu(beta,i,j,k)&
                                          *(grad_u(alpha,beta,i,j,k) + grad_u(beta,alpha,i,j,k))*ds
     end do

    end do

   end do
  end do
 end do
 !------------------------------------------------
 !-------lap_uなどの計算（全面周期）--------------
 do k=0,kmax
  do j=0,jmax
   do i=0,imax
    lap_u(i,j,k) = - 14.d0*u(i,j,k)
    lap_v(i,j,k) = - 14.d0*v(i,j,k)
    lap_w(i,j,k) = - 14.d0*w(i,j,k)

     !bulk部分
     if((i/=0).and.(i/=imax).and.(j/=0).and.(j/=jmax).and.(k/=0).and.(k/=kmax)) then
      do l=2,15
       lap_u(i,j,k) = lap_u(i,j,k) + u(i+ci(l),j+cj(l),k+ck(l))
       lap_v(i,j,k) = lap_v(i,j,k) + v(i+ci(l),j+cj(l),k+ck(l))
       lap_w(i,j,k) = lap_w(i,j,k) + w(i+ci(l),j+cj(l),k+ck(l))
      end do

     !境界（周期条件）
     else
      do l=2,15
       lap_u(i,j,k) = lap_u(i,j,k) + u(perx(i+ci(l)),pery(j+cj(l)),perz(k+ck(l)))
       lap_v(i,j,k) = lap_v(i,j,k) + v(perx(i+ci(l)),pery(j+cj(l)),perz(k+ck(l)))
       lap_w(i,j,k) = lap_w(i,j,k) + w(perx(i+ci(l)),pery(j+cj(l)),perz(k+ck(l)))
      end do
     end if

    lap_u(i,j,k) = lap_u(i,j,k) / (5.d0*ds**2)
    lap_v(i,j,k) = lap_v(i,j,k) / (5.d0*ds**2)
    lap_w(i,j,k) = lap_w(i,j,k) / (5.d0*ds**2)

   end do
  end do
 end do
 !------------------------------------------------
 !----laplap_uなどの計算（全面周期）--------------
 do k=0,kmax
  do j=0,jmax
   do i=0,imax
    laplap_u(i,j,k) = - 14.d0*lap_u(i,j,k)
    laplap_v(i,j,k) = - 14.d0*lap_v(i,j,k)
    laplap_w(i,j,k) = - 14.d0*lap_w(i,j,k)

     if((i/=0).and.(i/=imax).and.(j/=0).and.(j/=jmax).and.(k/=0).and.(k/=kmax)) then
      do l=2,15
       laplap_u(i,j,k) = laplap_u(i,j,k) + lap_u(i+ci(l),j+cj(l),k+ck(l))
       laplap_v(i,j,k) = laplap_v(i,j,k) + lap_v(i+ci(l),j+cj(l),k+ck(l))
       laplap_w(i,j,k) = laplap_w(i,j,k) + lap_w(i+ci(l),j+cj(l),k+ck(l))
      end do

     !境界（周期条件）
     else
      do l=2,15
       laplap_u(i,j,k) = laplap_u(i,j,k) + lap_u(perx(i+ci(l)),pery(j+cj(l)),perz(k+ck(l)))
       laplap_v(i,j,k) = laplap_v(i,j,k) + lap_v(perx(i+ci(l)),pery(j+cj(l)),perz(k+ck(l)))
       laplap_w(i,j,k) = laplap_w(i,j,k) + lap_w(perx(i+ci(l)),pery(j+cj(l)),perz(k+ck(l)))
      end do
     end if

    laplap_u(i,j,k) = laplap_u(i,j,k) / (5.d0*ds**2)
    laplap_v(i,j,k) = laplap_v(i,j,k) / (5.d0*ds**2)
    laplap_w(i,j,k) = laplap_w(i,j,k) / (5.d0*ds**2)

   end do
  end do
 end do
 !------------------------------------------------
 !---------grad_rhoの計算（全面周期）-------------
 do k=0,kmax
  do j=0,jmax
   do i=0,imax

    do alpha=1,3
     grad_rho(alpha,i,j,k) = 0.d0

     !bulk部分
     if((i/=0).and.(i/=imax).and.(j/=0).and.(j/=jmax).and.(k/=0).and.(k/=kmax)) then
      do l=2,15
       grad_rho(alpha,i,j,k) = grad_rho(alpha,i,j,k) + cr(alpha,l)*rho(i+ci(l),j+cj(l),k+ck(l))
      end do

     !境界（周期条件）
     else
      do l=2,15
       grad_rho(alpha,i,j,k) = grad_rho(alpha,i,j,k) &
                             + cr(alpha,l)*rho(perx(i+ci(l)),pery(j+cj(l)),perz(k+ck(l)))
      end do
     end if

     grad_rho(alpha,i,j,k) = grad_rho(alpha,i,j,k) / (10.d0*ds)

    end do

   end do
  end do
 end do
 !------------------------------------------------
 !---------normalの計算----------------------------
 do k=0,kmax
  do j=0,jmax
   do i=0,imax

    gtemp = dsqrt(grad_rho(1,i,j,k)**2+grad_rho(2,i,j,k)**2+grad_rho(3,i,j,k)**2)

     do alpha = 1, 3
      normal(alpha,i,j,k) = grad_rho(alpha,i,j,k)/(gtemp + epsilon)
     end do

   end do
  end do
 end do
 !------------------------------------------------
 !-----------chiの計算（全面周期）----------------
 do k=0,kmax
  do j=0,jmax
   do i=0,imax

     chi(i,j,k) = 0.d0

     !bulk部分
     if((i/=0).and.(i/=imax).and.(j/=0).and.(j/=jmax).and.(k/=0).and.(k/=kmax)) then
      do l=2,15
       do beta = 1,3
        chi(i,j,k) = chi(i,j,k) - cr(beta,l)*normal(beta,i+ci(l),j+cj(l),k+ck(l))
       end do
      end do

     !境界（周期条件）
     else
      do l=2,15
       do beta = 1,3
        chi(i,j,k) = chi(i,j,k) - cr(beta,l)*normal(beta,perx(i+ci(l)),pery(j+cj(l)),perz(k+ck(l)))
       end do
      end do
     end if

     chi(i,j,k) = chi(i,j,k) / (10.d0*ds)

   end do
  end do
 end do
 !------------------------------------------------
 !-----------徐々に界面張力を印加する-------------
 if(n<start_sigma) then
  sigma_temp = 0.d0
 else if(n>=start_sigma.and.n<=end_sigma) then
  sigma_temp = dble(n-start_sigma)/dble(end_sigma-start_sigma)*sigma
 else
  sigma_temp = sigma
 end if
 !------------------------------------------------
 !-----------fsvの計算----------------------------
 do k=0,kmax
  do j=0,jmax
   do i=0,imax

    do alpha = 1,3
     fsv(alpha,i,j,k) = sigma_temp*chi(i,j,k)*grad_rho(alpha,i,j,k)*rho(i,j,k)/((rhol-rhog)*(0.5d0*(rhog+rhol)))
    end do

   end do
  end do
 end do
 !------------------------------------------------
 !********流速の発展計算（全面周期）**************
  do k=0,kmax
   do j=0,jmax
    do i=0,imax

     unext(i,j,k) = -(1.d0-Au(i,j,k))/6.d0*lambda*ds**4*laplap_u(i,j,k) + fsv(1,i,j,k)/rho(i,j,k)*ds
     vnext(i,j,k) = -(1.d0-Au(i,j,k))/6.d0*lambda*ds**4*laplap_v(i,j,k) + fsv(2,i,j,k)/rho(i,j,k)*ds
     wnext(i,j,k) = -(1.d0-Au(i,j,k))/6.d0*lambda*ds**4*laplap_w(i,j,k) + fsv(3,i,j,k)/rho(i,j,k)*ds

     !bulk部分
     if((i/=0).and.(i/=imax).and.(j/=0).and.(j/=jmax).and.(k/=0).and.(k/=kmax)) then
      do l=1,15
       unext(i,j,k) = unext(i,j,k) + cr(1,l)*(delta_p(l,i,j,k) + geq(l,i-ci(l), j-cj(l), k-ck(l)) &
 				     +3.d0*Au(i,j,k)*E(l)*(cr(1,l)*(u(i,j,k)-u(i-ci(l), j-cj(l), k-ck(l))) &
                                                          +cr(2,l)*(v(i,j,k)-v(i-ci(l), j-cj(l), k-ck(l))) &
                                                          +cr(3,l)*(w(i,j,k)-w(i-ci(l), j-cj(l), k-ck(l)))) &
                               +3.d0*E(l)*(cr(1,l)*vcap(1,i,j,k)+cr(2,l)*vcap(2,i,j,k)+cr(3,l)*vcap(3,i,j,k)))


       vnext(i,j,k) = vnext(i,j,k) + cr(2,l)*(delta_p(l,i,j,k) + geq(l,i-ci(l), j-cj(l), k-ck(l)) &
  				     +3.d0*Au(i,j,k)*E(l)*(cr(1,l)*(u(i,j,k)-u(i-ci(l), j-cj(l), k-ck(l))) &
                                                          +cr(2,l)*(v(i,j,k)-v(i-ci(l), j-cj(l), k-ck(l))) &
                                                          +cr(3,l)*(w(i,j,k)-w(i-ci(l), j-cj(l), k-ck(l)))) &
                               +3.d0*E(l)*(cr(1,l)*vcap(1,i,j,k)+cr(2,l)*vcap(2,i,j,k)+cr(3,l)*vcap(3,i,j,k)))

       wnext(i,j,k) = wnext(i,j,k) + cr(3,l)*(delta_p(l,i,j,k) + geq(l,i-ci(l), j-cj(l), k-ck(l)) &
				     +3.d0*Au(i,j,k)*E(l)*(cr(1,l)*(u(i,j,k)-u(i-ci(l), j-cj(l), k-ck(l))) &
                                                          +cr(2,l)*(v(i,j,k)-v(i-ci(l), j-cj(l), k-ck(l))) &
                                                          +cr(3,l)*(w(i,j,k)-w(i-ci(l), j-cj(l), k-ck(l)))) &
                               +3.d0*E(l)*(cr(1,l)*vcap(1,i,j,k)+cr(2,l)*vcap(2,i,j,k)+cr(3,l)*vcap(3,i,j,k)))
      end do

     !境界（周期条件）
     else
      do l=1,15
       unext(i,j,k) = unext(i,j,k) &
                     + cr(1,l)*(delta_p(l,i,j,k) + geq(l,perx(i-ci(l)), pery(j-cj(l)), perz(k-ck(l))) &
 				     +3.d0*Au(i,j,k)*E(l)*(cr(1,l)*(u(i,j,k)-u(perx(i-ci(l)), pery(j-cj(l)), perz(k-ck(l)))) &
                                          +cr(2,l)*(v(i,j,k)-v(perx(i-ci(l)), pery(j-cj(l)), perz(k-ck(l)))) &
                                          +cr(3,l)*(w(i,j,k)-w(perx(i-ci(l)), pery(j-cj(l)), perz(k-ck(l))))) &
                               +3.d0*E(l)*(cr(1,l)*vcap(1,i,j,k)+cr(2,l)*vcap(2,i,j,k)+cr(3,l)*vcap(3,i,j,k)))

       vnext(i,j,k) = vnext(i,j,k) &
                    + cr(2,l)*(delta_p(l,i,j,k) + geq(l,perx(i-ci(l)), pery(j-cj(l)), perz(k-ck(l))) &
  				     +3.d0*Au(i,j,k)*E(l)*(cr(1,l)*(u(i,j,k)-u(perx(i-ci(l)), pery(j-cj(l)), perz(k-ck(l)))) &
                                          +cr(2,l)*(v(i,j,k)-v(perx(i-ci(l)), pery(j-cj(l)), perz(k-ck(l)))) &
                                          +cr(3,l)*(w(i,j,k)-w(perx(i-ci(l)), pery(j-cj(l)), perz(k-ck(l))))) &
                               +3.d0*E(l)*(cr(1,l)*vcap(1,i,j,k)+cr(2,l)*vcap(2,i,j,k)+cr(3,l)*vcap(3,i,j,k)))

       wnext(i,j,k) = wnext(i,j,k) &
                    + cr(3,l)*(delta_p(l,i,j,k) + geq(l,perx(i-ci(l)), pery(j-cj(l)), perz(k-ck(l))) &
				     +3.d0*Au(i,j,k)*E(l)*(cr(1,l)*(u(i,j,k)-u(perx(i-ci(l)), pery(j-cj(l)), perz(k-ck(l)))) &
                                          +cr(2,l)*(v(i,j,k)-v(perx(i-ci(l)), pery(j-cj(l)), perz(k-ck(l)))) &
                                          +cr(3,l)*(w(i,j,k)-w(perx(i-ci(l)), pery(j-cj(l)), perz(k-ck(l))))) &
                               +3.d0*E(l)*(cr(1,l)*vcap(1,i,j,k)+cr(2,l)*vcap(2,i,j,k)+cr(3,l)*vcap(3,i,j,k)))
      end do
     end if


    end do
   end do
  end do
 !************************************************
 !------------rhoとuの更新------------------------
  do k=0,kmax
   do j=0,jmax
    do i=0,imax
     rho(i,j,k) = rhonext(i,j,k)
     u(i,j,k) = unext(i,j,k)
     v(i,j,k) = vnext(i,j,k)
     w(i,j,k) = wnext(i,j,k)
    end do
   end do
  end do
 !------------------------------------------------
 !---------------------------------------------------------
 !---------------出力--------------------------------------
 !---------圧力差----------------------------
 phi_min = phi2
 phi_max = phi1
 veff = 0.d0

 do k=0,kmax
  do j=0,jmax
   do i=0,imax

    if(phi_min > phi(i,j,k)) then
     phi_min = phi(i,j,k)
     pg = p(i,j,k)
    end if

    if(phi_max < phi(i,j,k)) then
     phi_max = phi(i,j,k)
     pl = p(i,j,k)
    end if

    if(phi(i,j,k) >= 0.5d0*(phi1+phi2)) then
     veff = veff + ds**3
    end if

   end do
  end do
 end do

 reff = (3.d0/4.d0*veff / pi)**(1.d0/3.d0)
 dp_th = 2.d0*sigma/reff
 dp_calc = pl-pg
 err = dabs(dp_calc-dp_th)/dp_th

 write(8,'(1i10, 1x, 3f20.15)') n, dp_calc, dp_th, err
 !-------------------------------------------
 !---------実効液滴体積・直径----------------
 write(9,'(1i10, 1x, 2f20.15)') n, veff/vinput, reff/rinput
 !-------------------------------------------
 !---phi，密度，圧力，流速分布（gnuplot用）--
  if(mod(n, step/100) == 0) then

    num='      '
    write(num,'(1i6)') n
    do j=1,6
     if(num(j:j)==' ') num(j:j)='0'
    end do

    open(11,file='flow'//num//'.txt')


    k=int(0.5d0*kmax)

    do j=0, jmax
     do i=0, imax
      write(11,'(2f20.15, 1x, 6f20.15)') i*ds/h0, j*ds/h0, &
                                         phi(i,j,k), rho(i,j,k), p(i,j,k), u(i,j,k), v(i,j,k), w(i,j,k)
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
 close(9)


 contains

 !====================================================================周期用関数
 !-------------------------------------
 function perx(i)
  integer:: perx
  integer, intent(in):: i

  if(i<0) then
   perx = i+imax
  else if(i>imax) then
   perx = i-imax
  else
   perx = i
  end if

 end function perx
 !-------------------------------------
 !-------------------------------------
 function pery(j)
  integer:: pery
  integer, intent(in):: j

  if(j<0) then
   pery = j+jmax
  else if(j>jmax) then
   pery = j-jmax
  else
   pery = j
  end if

 end function pery
 !-------------------------------------
 !-------------------------------------
 function perz(k)
  integer:: perz
  integer, intent(in):: k

  if(k<0) then
   perz = k+kmax
  else if(k>kmax) then
   perz = k-kmax
  else
   perz = k
  end if

 end function perz
 !-------------------------------------

 !====================================================================

end program laplace
