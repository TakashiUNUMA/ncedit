# ncedit
======

多次元 NetCDF ファイルから GMT で描画用の 2次元 NetCDF をはき出すプログラム


## コンパイル
NetCDF ver. 4.1.3, HDF5 ver. 1.8.7, ZLIB 1.2.5 を事前にコンパイルしておいて下さい。
上記のライブラリの準備は、[こちら](https://github.com/TakashiUNUMA/wrflib_instsh) を参照。

Makefile を編集する。
Intel, PGI, GNU fortran コンパイラのどれかを選択します。

編集が完了したら make します。
```
 $ make
```

ncedit

というバイナリが作成されていたら OK 。


## 使用方法
基本的には、namelist.ncedit を編集しながら使用する。

```
&param
 imax               = 256          ! x 方向の grid 数
 jmax               = 3            ! y 方向の grid 数
 kmax               = 64           ! z 方向の grid 数
 tmax               = 361          ! t 方向の grid 数
 varname            = "water"      ! NetCDF の変数名 或いは ncedit.f90 内で定義した変数名
 input              = "input.nc4"  ! 入力する 多次元 NetCDF ファイル
 xselect            = 128          ! 出力する 2 次元 NetCDF ファイルで選択される grid 番号
 yselect            = 2            ! 出力する 2 次元 NetCDF ファイルで選択される grid 番号
 zselect            = 1            ! 出力する 2 次元 NetCDF ファイルで選択される grid 番号
 tselect            = 240          ! 出力する 2 次元 NetCDF ファイルで選択される grid 番号
 output             = "water.nc4"  ! 出力する 2 次元 NetCDF ファイル名
 flag               = 2            ! 出力する次元 1: x-y, 2: x-z, 3: x-t (x-y はまだきちんと書いてない)
 ny                 = 41           ! 出力する 2 次元 NetCDF ファイルの y 軸方向の grid 数 (x-z 断面用)
 dy                 = 0.500        ! 出力する 2 次元 NetCDF ファイルの y 軸方向の格子間隔 (x-z 断面用)
 deflate_level      = 2            ! NetCDF4 の deflate level
 debug_level        = 100          ! デバッグレベル
/
```

例えば、4 次元データを x-z の 2 次元に次元をおとして出力する場合は、以下のようにする。
```
 flag = 2
 tselect = 240 (計算開始から 240 ステップ目)
 yselect = 2   (y 軸の 2 grid 目)
 dy                 = 0.500        ! 元のデータが stretch なので、0.5 km の等格子間隔に直す
 ny                 = 41           ! その時の grid 数
```

ncedit.f90 を編集しやすくしたことで、出来るだけ汎用性の高いものをつくろうとしているところです。

何か質問・提案等ございましたら、 kijima.m.u (at) gmail.com へどうぞ。
