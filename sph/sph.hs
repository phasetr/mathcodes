{-
https://github.com/takagi/blog-codes/blob/master/20091101/haskell/sph.hs

ToDo:
  □【重要】近傍マップは領域より一回り大きくとる。
    粒子がさらにその外に来た場合にはエラーとする。
  □subversionで管理
  □近傍マップをcalc_amountとadvanceとで２回作るのがヤダ
  □だんだん１ステップの計算速度が遅くなっていないか？
  □cppと比べてどれくらい遅いか？
    □cppでかかる時間は？
    □hsでかかる時間は？
  □ユニットテスト
  □OpenGL
  □マーチングキューブ
  □関数の整理
  □プロファイラを使ってチューニング
  □近傍マップ関連のデータ型を整理する
  ■boundary_condition
  ■output
  ■massの導入
    - Particleの定義にmassを入れる
    - Particleの初期化にmassを入れる
    - advanceで新しいParticleを作るときにmassを入れる
    - 速度を力から計算する（つまり、aではなくfから作る）
    - など
  ■複数粒子の導入
    - 初期状態の型を[Particle]にする
    - simulationの型を [Particle] -> [[Particle]] にする
    - advanceの型を [Particle] -> [Particle] にする
  ■初期配置->もっと粒子数が少なくて済むようにする
  ■近傍マップ
  ■sph_hはsimscaleをかけた後のもの？
    →その前提で書いている
  ■位置や速度がNaNになってしまう問題の解決
    →dtが大きすぎて、近傍マップの範囲外に粒子が移動してしまっていたから
  ×massは不要
  ■近傍マップの作成が遅すぎる
-}

import Data.Vector
import Data.Array
import System

{-
  Constants
-}
dt          = 0.004
simscale    = 0.004
sph_h       = 0.01
visc        = 0.2
restdensity = 600.0
intstiff    = 3.0
pmass       = 0.00020543
pdist       = (pmass / restdensity) ** (1/3)
e           = 2.71828183
radius      = 0.004
extstiff    = 10000.0
extdamp     = 256.0
epsilon     = 0.00001
limit       = 200.0
g           = Vector3 0 (-9.8) 0
box_min     = Vector3 0  0 (-10)
--box_max     = Vector3 300 50 10
box_max     = Vector3 20 50 10
init_min    = Vector3 0  0 (-10)
init_max    = Vector3 10 20 10

main = do n <- getArgs
          putNumberOf ps0
          output $ take (read (n!!0)) $ simulation ps0

-- | 粒子の定義
data Particle =
  Particle { pos, vel :: {-# UNPACK #-} !Vector3,
             mass, rho, prs :: {-# UNPACK #-} !Scalar }
  deriving (Show, Eq)

ps0 :: [Particle]
ps0 = [ Particle
        { pos  = Vector3 x y z,
          vel  = Vector3 0 0 0,
          mass = pmass,
          rho  = 0,
          prs  = 0 }
      | x <- xs, y <- ys, z <- zs ]
  where
    d  = pdist / simscale * 0.95
    xs = let minx = v3x init_min
             maxx = v3x init_max
         in [minx+d, minx+d+d .. maxx-d]
    ys = let miny = v3y init_min
             maxy = v3y init_max
         in [miny+d, miny+d+d .. maxy-d]
    zs = let minz = v3z init_min
             maxz = v3z init_max
         in [minz+d, minz+d+d .. maxz-d]
{-
ps0 = [ Particle { pos = initial_position x y z,
                   vel = (simscale*30.0) *<> (initial_velocity x y z),
                   mass = pmass,
                   rho  = 0,
                   prs  = 0  }
        | x <- xs, y <- ys, z <- zs, is_under_surface x y z ]
  where d  = pdist / simscale * 0.95
        xs = let minx = v3x box_min
                 maxx = v3x box_max
             in [minx+d, minx+d+d .. maxx]
        ys = let miny = v3y box_min
                 maxy = v3y box_max
             in [miny+d, miny+d+d .. maxy]
        zs = let minz = v3z box_min
                 maxz = v3z box_max
             in [minz+d, minz+d+d .. maxz]
        is_under_surface x y _
          | y > d+h*_sech**2 = False
          | x <= 50          = y >= 0.0
          | x <= 150         = -x+100/18*y+50 >= 0.0
          | otherwise        = if y < 18 then False else True
          where _sech = sech((sqrt((3*h)/(4*d**3)))*(x-20))
                h     = 7
                d     = 10
                g     = 9.8
        initial_position x y z = Vector3 x y z
        initial_velocity x y _ =
            let _x = (sqrt(g*d)) * h/d * _sech**2
                _y = (sqrt(g*d)) * (h/d)**1.5 * y/d * _sech**2 * _tanh
                h  = 7
                d  = 10
                g  = 9.8
                _sech = sech((sqrt((3*h)/(4*d**3)))*(x-20))
                _tanh = Main.tanh((sqrt((3*h)/(4*d**3)))*(x-20))
            in Vector3 _x _y 0

sech :: Scalar -> Scalar
sech x = 2.0 / (e**x + e**(-x))

tanh :: Scalar -> Scalar
tanh x = (e**x - e**(-x)) / (e**x + e**(-x))
-}

-- | NeighborMap の定義
type NeighborIdx = (Int,Int,Int)
type NeighborMap = Array NeighborIdx [Particle]
type NeighborList = [(NeighborIdx,[Particle])]

neighbor_idx :: Vector3 -> NeighborIdx
neighbor_idx (Vector3 x y z) =
  (toIx x, toIx y, toIx z)
  where
    toIx :: Scalar -> Int
    toIx x = floor(x*simscale/sph_h)

neighbor_map :: [Particle] -> NeighborMap
neighbor_map ps =
  let ix_min   = let (ix,iy,iz) = neighbor_idx box_min
                 in (ix-1,iy-1,iz-1)
      ix_max   = let (ix,iy,iz) = neighbor_idx box_max
                 in (ix+1,iy+1,iz+1)
      nbr_list = _neighbor_list ps []
  in array (ix_min,ix_max)
     [ (ix,case lookup ix nbr_list of
             Just x  -> x
             Nothing -> [])
     | ix <- range (ix_min,ix_max) ]
  where
    _neighbor_list :: [Particle] -> NeighborList -> NeighborList
    _neighbor_list [] lst = lst
    _neighbor_list (p:ps) lst =
      insert_neighbor p (_neighbor_list ps lst)

insert_neighbor :: Particle -> NeighborList -> NeighborList
insert_neighbor p@(Particle {pos=r}) nbr_map =
  let ixp = neighbor_idx r
  in _insert_neighbor ixp p nbr_map
  where
    _insert_neighbor :: NeighborIdx -> Particle -> NeighborList
                     -> NeighborList
    _insert_neighbor ixp p (x@(ixx,ps):xs)
      | ixp == ixx = (ixx,(p:ps)) : xs
      | otherwise  = x : _insert_neighbor ixp p xs
    _insert_neighbor ixp p [] = [(ixp,[p])]

neighbor :: NeighborMap -> Vector3 -> [Particle]
neighbor nbr_map r =
  let (_ix,_iy,_iz) = neighbor_idx r
      ix_min   = let (ix,iy,iz) = neighbor_idx box_min
                 in (ix-1,iy-1,iz-1)
      ix_max   = let (ix,iy,iz) = neighbor_idx box_max
                 in (ix+1,iy+1,iz+1)
  in flatten [ nbr_map!(ix,iy,iz)
               | ix <- [_ix-1, _ix, _ix+1]
               , iy <- [_iy-1, _iy, _iy+1]
               , iz <- [_iz-1, _iz, _iz+1]
               , ix_min <= (ix,iy,iz)
               , (ix,iy,iz) <= ix_max ]

-- | シミュレーション用の関数
simulation :: [Particle] -> [[Particle]]
simulation ps =
  let ps'  = calc_amount ps $ neighbor_map ps
      ps'' = advance ps' $ neighbor_map ps'
  in ps : simulation ps''

calc_amount :: [Particle] -> NeighborMap -> [Particle]
calc_amount ps nbr_map = map _calc_amount ps
  where
    _calc_amount p@(Particle r v m _ _) =
      let rho = density (neighbor nbr_map r) p
          prs = (rho - restdensity ) * intstiff
      in Particle r v m rho prs

advance :: [Particle] -> NeighborMap -> [Particle]
advance ps nbr_map = map _advance ps
  where
    _advance p@(Particle r v m rho prs) =
      let r_next = r + v_next * dt / fromScalar(simscale)
          v_next = v + a * dt
          a      = g + (boundary_condition p _a)
          _a     = f / fromScalar(rho)
          f      = force (neighbor nbr_map r) p
      in Particle r_next v_next m rho prs

density :: [Particle] -> Particle -> Scalar
density ps i = sum $ map exp ps
  where
    exp :: Particle -> Scalar
    exp j = let Particle { pos = r_i } = i
                Particle { pos = r_j, mass = m_j } = j
                dr = simscale *<> (r_i - r_j)
            in m_j * (poly6_kernel dr)

force :: [Particle] -> Particle -> Vector3
force ps p =
  pressure_term ps p + viscosity_term ps p

pressure_term :: [Particle] -> Particle -> Vector3
pressure_term ps i =
  -(sum $ map exp $ filter (/=i) ps)
  where
    exp :: Particle -> Vector3
    exp j = let Particle { pos = r_i, prs = p_i    } = i
                Particle { pos = r_j, prs = p_j,
                           mass = m_j, rho = rho_j } = j
                dr = simscale *<> (r_i - r_j)
            in (m_j * (p_i + p_j) / (2.0*rho_j))
                                    *<> (grad_spiky_kernel dr)

viscosity_term :: [Particle] -> Particle -> Vector3
viscosity_term ps i =
  visc * (sum $ map exp $ filter (/=i) ps)
  where
    exp :: Particle -> Vector3
    exp j = let Particle { pos = r_i, vel = v_i    } = i
                Particle { pos = r_j, vel = v_j,
                           mass = m_j, rho = rho_j } = j
                dr = simscale *<> (r_i - r_j)
            in (m_j / rho_j * (rap_visc_kernel dr)) *<> (v_j - v_i)

boundary_condition :: Particle -> Vector3 -> Vector3
boundary_condition p@Particle {pos=r,vel=v} a =
  x_wall_max $ x_wall_min
  $ y_wall_max $ y_wall_min
  $ z_wall_max $ z_wall_min
  $ accel_limit a
  where
    x_wall_max = wall ((v3x box_max)-(v3x r)) (Vector3 (-1) 0 0)
    x_wall_min = wall ((v3x r)-(v3x box_min)) (Vector3 1 0 0)
    y_wall_max = wall ((v3y box_max)-(v3y r)) (Vector3 0 (-1) 0)
    y_wall_min = wall ((v3y r)-(v3y box_min)) (Vector3 0 1 0)
    z_wall_max = wall ((v3z box_max)-(v3z r)) (Vector3 0 0 (-1))
    z_wall_min = wall ((v3z r)-(v3z box_min)) (Vector3 0 0 1)
    wall d norm a = let diff = 2.0*radius - d*simscale
                        adj  = extstiff*diff-extdamp*(vdot norm v)
                    in if diff > epsilon
                      then a + (adj *<> norm)
                      else a
    accel_limit a = let speed  = vmag a
                    in if speed > limit
                      then (limit / speed) *<> a
                      else a

{-
  Kernel functions
-}

poly6_kernel :: Vector3 -> Scalar
poly6_kernel x
  | r <= sph_h = 315.0 / (64.0*pi*(sph_h**9)) * (sph_h**2-r**2)**3
  | otherwise  = 0
  where
    r = vmag x

grad_spiky_kernel :: Vector3 -> Vector3
grad_spiky_kernel x
  | r <= sph_h = ((-45.0) / (pi*(sph_h**6)) * ((sph_h-r)**2) / r) *<> x
  | otherwise  = Vector3 0 0 0
  where
    r = vmag x

rap_visc_kernel :: Vector3 -> Scalar
rap_visc_kernel x
  | r <= sph_h = 45.0 / (pi*(sph_h**6)) * (sph_h-r)
  | otherwise  = 0
  where
    r = vmag x


{-
  Utilities
-}

flatten :: [[a]] -> [a]
flatten = foldr (++) []


{-
  Actions and functions for output
-}

putNumberOf :: Show a => [a] -> IO()
putNumberOf = putStrLn . show . length

output :: [[Particle]] -> IO()
output pss = _output pss 0
  where
    _output :: [[Particle]] -> Int -> IO()
    _output (ps:pss) i = do output_povs ps i
                            --output_csvs ps i
                            _output pss (i+1)
    _output []       _ = return ()

output_povs :: [Particle] -> Int -> IO()
output_povs ps i =
  do putStrLn ("processing "++ (file_name i)++" ...")
     write ps i
  where
    write :: [Particle] -> Int -> IO()
    write ps i = writeFile (file_name i) (body ps)

    file_name :: Int -> String
    file_name i = "result" ++ (rjust8 i) ++ ".pov"

    body :: [Particle] -> String
    body ps = "#include \"colors.inc\"\n"
           ++ "camera {\n"
           ++ "  location <10, 30, -40.0>\n"
           ++ "  look_at <10, 10, 0.0>\n"
           ++ "}\n"
           ++ "light_source { <0, 30, -30> color White }\n"
           ++ (flatten $ map sphere ps)

    sphere :: Particle -> String
    sphere p = let Vector3 x y z = pos p
               in "sphere {\n"
               ++ "  <"++(show x)++","++(show y)++","++(show z)
                                                        ++">,0.5\n"
               ++ "  texture {\n"
               ++ "    pigment { color Yellow }\n"
               ++ "  }\n"
               ++ "}\n";

output_csvs :: [Particle] -> Int -> IO()
output_csvs ps i =
  do putStrLn ("processing "++ (file_name i)++" ...")
     write ps i
  where
    write :: [Particle] -> Int -> IO()
    write ps i = writeFile (file_name i) (header++body ps)

    file_name :: Int -> String
    file_name i = "result" ++ (rjust8 i) ++ ".csv"

    header :: String
    header = "x,y,z,vx,vy,vz,rho,prs\n"

    body :: [Particle] -> String
    body ps = flatten $ map line ps

    line :: Particle -> String
    line Particle {pos = (Vector3 x y z),
                   vel = (Vector3 vx vy vz),
                   rho = rho,
                   prs = prs}
         = (flatten $ map (\x -> show x ++ ",")
                          [x,y,z,vx,vy,vz,rho,prs]) ++ "\n"

rjust :: Int -> Int -> String
rjust width s =
  replicate (width - (length $ show s)) '0' ++ (show s)
rjust8 =
  rjust 8
