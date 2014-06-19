# ncedit

__多次元 NetCDF ファイルから GMT で描画用の 2次元 NetCDF をはき出すプログラム__


## コンパイル
NetCDF ver. 4.1.3, HDF5 ver. 1.8.7, ZLIB ver. 1.2.5 を事前にコンパイル・インストールしておいて下さい。

上記のライブラリの準備は、[こちら](https://github.com/TakashiUNUMA/wrflib_instsh) を参照。


ライブラリの準備が整ったら、Makefile を編集します。

Intel, PGI, GNU fortran コンパイラのどれかを選択します。

ここでは例として PGI コンパイラを選択します (コメントアウトして下さい)。
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


## 使用方法
基本的には、namelist.ncedit or namelist.ncedit_stats を編集し、使用します (プログラム本体をあまり弄らないで良いようにするため)。

```
&param
 imax               = 128          ! x 方向の grid 数
 jmax               = 128          ! y 方向の grid 数
 kmax               = 64           ! z 方向の grid 数
 tmax               = 61           ! t 方向の grid 数
 varname            = "water"      ! NetCDF の変数名 或いは ncedit.f90 内で定義した変数名
 input              = "input.nc4"  ! 入力する 多次元 NetCDF ファイル
 xselect            = 64           ! 出力する 2 次元 NetCDF ファイルで選択される grid 番号
 yselect            = 64           ! 出力する 2 次元 NetCDF ファイルで選択される grid 番号
 zselect            = 20           ! 出力する 2 次元 NetCDF ファイルで選択される grid 番号
 tselect            = 60           ! 出力する 2 次元 NetCDF ファイルで選択される grid 番号
 output             = "water.nc4"  ! 出力する 2 次元 NetCDF ファイル名
 flag               = 2            ! 出力する次元 1: x-y, 2: x-z, 3: x-t (x-y はまだきちんと書いてない)
 nx                 = 128          ! 出力する 2 次元 NetCDF ファイルの x 軸方向の grid 数 (x-y, x-z 断面用)
 ny                 = 41           ! 出力する 2 次元 NetCDF ファイルの y 軸方向の grid 数 (x-y, x-z 断面用)
 dy                 = 0.500        ! 出力する 2 次元 NetCDF ファイルの y 軸方向の格子間隔 (x-z 断面用)
 deflate_level      = 2            ! NetCDF4 の deflate level
 debug_level        = 100          ! デバッグレベル
/
```

例1: 4 次元データを x-y 断面として出力する場合
```
 flag    = 1      ( x-y 断面 )
 zselect = 20     ( z 軸の 20 grid 目 )
 tselect = 60     ( 計算開始から 60 ステップ目 )
 nx      = 128    ( grid 数: x 軸 )
 ny      = 128    ( grid 数: y 軸 )
```

例2: 4 次元データを x-z 断面として出力する場合
```
 flag    = 2      ( x-z 断面 )
 yselect = 2      ( y 軸の 2 grid 目 )
 tselect = 240    ( 計算開始から 240 ステップ目 )
 nx      = 256    ( grid 数: x 軸 )
 dy      = 0.500  ( 元のデータが stretch だったので、0.5 km の等格子間隔に直すためのもの )
 ny      = 41     ( 上記の格子間隔での grid 数: y 軸 )
```

例3: 4 次元データを x-t 断面として出力する場合
```
 flag    = 3      ( x-t 断面 )
 yselect = 2      ( y 軸の 2 grid 目 )
 zselect = 1      ( z 軸の 1 grid 目 )
 nx      = 128    ( grid 数: x 軸 )
 ny      = 128    ( grid 数: t 軸 )
```

また、ncedit.f90 は、

- 多次元 -> 2 次元出力
- 出力時の単位変換
- 元データに無い変数の計算

等のため、出来るだけ単純化しているつもりです。
今後はこの単純な構造を保ちつつ複雑にならないように設計していくつもりです。


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
[数値解析用 Fortran 90 ライブラリ (STPK)](http://www.gfd-dennou.org/library/davis/stpk/) の一部を使用させていただきました。
開発者の 辻野 智紀 氏に感謝申し上げます。


# 参考文献・URL
- http://www.u.tsukuba.ac.jp/~hayasaki.masamits.fw/Linux_tips/GMT_img_script/grdvector.gmt
- http://www.unidata.ucar.edu/software/netcdf/docs/netcdf-f90/NF90_005fGET_005fVAR.html#NF90_005fGET_005fVAR


# TODO
- x-y 断面の拡張


# その他
何か質問・提案等ございましたら、 kijima.m.u (at) gmail.com へどうぞ。
