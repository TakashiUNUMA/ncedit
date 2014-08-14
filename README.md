# ncedit

__多次元 NetCDF ファイルから GMT で描画用の 2次元 NetCDF をはき出すプログラム__


## コンパイル
NetCDF ver. 4.1.3, HDF5 ver. 1.8.7, ZLIB ver. 1.2.5 を事前にコンパイル・インストールしておいて下さい。

上記のライブラリの準備は、[こちら](https://github.com/TakashiUNUMA/wrflib_instsh) を参照。


### Make 編
ライブラリの準備が整ったら、Makefile を編集します。

Intel, PGI, GNU fortran コンパイラのどれかを選択します (使用したいコンパイラの行をコメントアウトして下さい)。

ここでは例として PGI コンパイラを選択します。
```
#COMPILER=INTEL
#COMPILER=GNU
COMPILER=PGI
```

注意：使用するコンパイラは、コンパイル済みのライブラリで使用したコンパイラと一致している必要が有ります。


編集が完了したら make します。
```
 $ make
```

ncedit, ncedit_stats

というバイナリが作成されていたら OK 。


### SCons 編

ライブラリの準備が整ったら、SConstruct を編集します。

Intel, PGI, GNU fortran コンパイラのどれかを選択します (使用したいコンパイラの行をコメントアウトして下さい)。

ここでは例として GNU コンパイラを選択します。
```
#COMPILER=INTEL
COMPILER=GNU
#COMPILER=PGI
```

注意：使用するコンパイラは、コンパイル済みのライブラリで使用したコンパイラと一致している必要が有ります。


編集が完了したら make します。
```
 $ scons
```

ncedit, ncedit_stats

というバイナリが作成されていたら OK 。


## 使用方法
namelist.ncedit または namelist.ncedit_stats を編集し、使用します。

```
&param
 imax               = 256          ! x 方向の grid 数
 jmax               = 256          ! y 方向の grid 数
 kmax               = 64           ! z 方向の grid 数
 tmax               = 37           ! t 方向の grid 数
 varname            = "water"      ! NetCDF の変数名 或いは ncedit.f90 内で定義した変数名
 input              = "input.nc4"  ! 入力する 多次元 NetCDF ファイル
 xselect            = 128          ! 出力する 2 次元 NetCDF ファイルで選択される grid 番号
 yselect            = 128          ! 出力する 2 次元 NetCDF ファイルで選択される grid 番号
 zselect            = 20           ! 出力する 2 次元 NetCDF ファイルで選択される grid 番号
 tselect            = 18           ! 出力する 2 次元 NetCDF ファイルで選択される grid 番号
 output             = "water.nc4"  ! 出力する 2 次元 NetCDF ファイル名
 output_type        = "nc4"        ! 出力する 2 次元 NetCDF の種類 nc3: netcdf_classic, nc3_64bit: netcdf_64bit_offset, nc4: netcdf4
 flag               = 2            ! 出力する断面 1: x-y, 2: x-z, 3: x-t, 4: t, 5: 任意鉛直
 nx                 = 128          ! 出力する 2 次元断面の x 軸方向の grid 数 (x-y, x-z 断面用)
 ny                 = 41           ! 出力する 2 次元断面の y 軸方向の grid 数 (x-y, x-z 断面用)
 dx                 = 0.500        ! 出力する 2 次元断面の x 軸方向の格子間隔 (x-y, x-z, x-t 断面用)
 dy                 = 0.500        ! 出力する 2 次元断面の y 軸方向の格子間隔 (x-z 断面用)
 angle              = 45.0         ! 任意の鉛直断面と x 軸と成す角度 [deg] (for flag = 5)
 interp_x           = 0            ! 出力する 2 次元断面の x 軸の内挿の有無 0: 内挿しない, 1: 内挿する
 interp_y           = 0            ! 出力する 2 次元断面の y 軸の内挿の有無 0: 内挿しない, 1: (interp_method で) 内挿する
 interp_method	    = 'linear'     ! x-z 断面出力時の y 軸の内挿方法
                                   !  'linear': (1 次元) 線形内挿
                                   !  'near'  : 最近傍探索 (内挿はしない)
                                   !  'stpk'  : 最近傍探索 (内挿はしない)
 deflate_level      = 2            ! NetCDF4 の deflate level
 debug_level        = 100          ! デバッグレベル (数値を大きくするほど，実行過程の詳細が出力される)
/
```

例1: 4 次元データを x-y 断面として出力する場合
```
 flag               = 1        ( x-y 断面 )
 zselect            = 20       ( z 軸の 20 grid 目 )
 tselect            = 60       ( 計算開始から 60 ステップ目 )
 nx                 = 128      ( grid 数: x 軸 )
 ny                 = 128      ( grid 数: y 軸 )
```

例2: 4 次元データを x-z 断面として出力する場合
```
 flag               = 2        ( x-z 断面 )
 yselect            = 2        ( y 軸の 2 grid 目 )
 tselect            = 240      ( 計算開始から 240 ステップ目 )
 nx                 = 256      ( 出力データの x 軸 grid 数 )
 ny                 = 41       ( 出力データの y 軸 grid 数 )
 dx                 = 0.500    ( 元データが stretch の場合、0.5 km の等格子間隔に直す )
 dy                 = 0.500    ( 元データが stretch の場合、0.5 km の等格子間隔に直す )
 interp_x           = 1        ( x 軸座標の内挿 )
 interp_y           = 1        ( y 軸座標の内挿 )
 interp_method      = 'linear' ( y 軸の内挿方法 )
```

例3: 4 次元データを x-t 断面として出力する場合
```
 flag               = 3        ( x-t 断面 )
 yselect            = 2        ( y 軸の 2 grid 目 )
 zselect            = 1        ( z 軸の 1 grid 目 )
 nx                 = 128      ( grid 数: x 軸 )
 ny                 = 128      ( grid 数: t 軸 )
 interp_x           = 0        ( x 軸座標をそのまま出力 )
 interp_y           = 0        ( y 軸座標をそのまま出力 )
```

例4: 4 次元データを x-z 任意断面として出力する場合
```
 flag               = 5        ( x-z 任意断面 )
 xselect            = 0        ( 投影水平軸の中心座標位置 )
 yselect            = 0        ( 投影水平軸の中心座標位置 )
 tselect            = 240      ( 計算開始から 240 ステップ目 )
 nx                 = 256      ( 出力データの x 軸 grid 数 )
 ny                 = 41       ( 出力データの y 軸 grid 数 )
 dy                 = 0.500    ( 元データが stretch の場合、0.5 km の等格子間隔に直す )
 angle              = 45.0     ( 投影面の x 軸からの角度 )
 interp_y           = 1        ( y 軸座標の内挿 )
 interp_method      = 'linear' ( y 軸の内挿方法 )
```


ncedit.f90 は、

- 多次元 -> 2 次元出力
- 出力時の単位変換
- 元データに無い変数の計算

等のため、出来るだけ単純化しているつもりです (が、既に複雑になってしまいました)。


## 出力データの確認 (ncdump, ncview 等)
```
 $ ncdump -h hoge.nc4
```
や
```
 $ ncview hoge.nc4
```
等で簡易確認出来ます。


## 出力データの描画 (GMT)
以下の 4 つのタイプを用意しています。
- gmtplot_xy.sh      # ncedit.f90 で出力した x-y 断面データ用
- gmtplot_xz.sh      # ncedit.f90 で出力した x-z 断面データ用
- gmtplot_xt.sh      # ncedit.f90 で出力した x-t 断面データ用
- gmtplot_stats.sh   # ncedit_stats.f90 で出力した一次元データ用


# 謝辞
- 辻野 智紀 氏が開発している、[数値解析用 Fortran 90 ライブラリ (STPK)](http://www.gfd-dennou.org/library/davis/stpk/) の一部を使用させていただきました。
- Gorge Bryan 氏の CAPE 計算サブルーチン (getcape.F) を使用させていただきました。

関係者各位に感謝申し上げます。


# 参考文献・URL
- http://www.unidata.ucar.edu/software/netcdf/docs/netcdf-f90/NF90_005fGET_005fVAR.html#NF90_005fGET_005fVAR
- http://www.u.tsukuba.ac.jp/~hayasaki.masamits.fw/Linux_tips/GMT_img_script/grdvector.gmt
- http://www.scons.org/doc/production/HTML/scons-user.html


# TODO


# MEMO
- Ubuntu 14.04 では，apt-get install scons で取得した scons ver. 2.3.0 で正常動作中
- CentOS 5.X, 6.X 系では，Python ver. 2.7.8 + scons ver. 2.3.2 の組み合わせで正常動作中
- NetCDF 関連のライブラリリンクは非常に良好 (GNU make より楽)
- Python のバージョンを下げて (e.g, ver. 2.6.6) scons ver. 2.3.2 をインストールすると，実行時にエラーを吐いて正常に動作しない
- PGI, Intel fortran compiler を使用するときは，絶対パスでコンパイラを指定する必要がある (SConstruct 内部の PATH の問題？)
