# Common LispでRaspberry Pi電子工作

## はじめに

Raspberry Pi は、電子工作もできる小型PCです。
主に教育現場や安価に入手できる IOT 機器として趣味や業務に使われています。
Raspberry Pi で電子工作と言えば、 Python を使って紹介している記事や本がとても多いですが、 Common Lisp でも同じようにすることができます。
Common Lisp で電子工作するメリットは、C言語ほどではないですがそこそこ実行速度が早いことや、 REPL で簡単に動作を確認できることなどがあります。
今回は、開発環境の構築方法と簡単な電子工作の例をいくつか挙げて Common Lisp を使った Raspberry Pi 電子工作について紹介したいと思います。

## 今回使用したRaspberry PiとOS

ハードは`Raspberry Pi 3`、OSは`Raspbian Stretch`を使用します。
`Raspbian Stretch`は以下のミラーサイトを使うと公式サイトよりも早くダウンロード出来ます。

```
ミラーサイトURL：http://ftp.jaist.ac.jp/pub/raspberrypi/raspbian/images/
```

今回は執筆時での最新版`raspbian-2018-11-15`を使用しています。

## Common Lisp環境構築

Common Lisp環境構築にはRoswellを使用しました。
Roswellは基本的に`homebrew (Linuxではlinuxbrew)`でインストールしますが、`homebrew`がRaspberry PiのCPUであるARM32をサポートしていないため、以下に示す手順でソースコードをビルドしてインストールします。
また、ARM32のSBCL(Steel Bank Common Lisp)がスレッド対応していないため、今回はCCL (Clozure Common Lisp)を使用します。

## GPIO制御ライブラリについて

GPIO制御ライブラリとして`Wiring Pi`を使用しています。
これは、`Raspbian Stretch`には最初からインストールされているので別途インストールする必要はありません。
Common LispからはCFFIでラッパーを作成して呼び出しています。
CFFIとは、Common LispからC言語で作成された関数を呼び出すためのインターフェースです。

```
- WiringPi
  公式サイト：http://wiringpi.com/

- CFFI
  公式サイト：https://common-lisp.net/project/cffi/
  ユーザーマニュアル：https://common-lisp.net/project/cffi/manual/index.html
```

## LED

最初は電子工作の基本、LEDを点滅させるLチカをやってみます。

### 使用した電子部品と回路図

電子部品は次のものを使用しました。

- 赤色LED 1個
- 330Ω抵抗(橙橙茶金) 1個

上記電子部品を以下のようにブレッドボードに配置します。

![回路図](images/09-circuit-diagram-blink.jpg)

### プログラム本体作成

プログラム本体を`src`ディレクトリ内に`blink.lisp`という名前で作成します。

```
(defpackage :cl-raspi/src/blink
  (:use :cl
        :cl-raspi/lib-wiring-pi)
  (:export :main))
(in-package :cl-raspi/src/blink)

(defconstant +pin+ 11)

(defun main ()
  (wiringpi-setup-gpio)
  (pin-mode +pin+ +output+)

  (loop
     (digital-write +pin+ 1)
     (delay 500)
     (digital-write +pin+ 0)
     (delay 500)))
```

最初に`cl-raspi/src/blink`という名前でパッケージを定義し、`cl-raspi/lib-wiring-pi`を読み込みます。
また、外部から`main`関数を参照できるように`export`しておきます。
LEDをつないだGPIOピン番号を`defconstant`で定数定義します(今回はGPIO11を使用)。
プログラム本体は`main`関数として書いていきます。
流れとしては、以下の通りです。

1. `wiringpi-setup-gpio`でGPIOを初期化
2. `pin-mode`でGPIO11を出力モードに設定
3. 無限ループ内で`digital-write`を使ってGPIO11の電圧のHigh(1)/Low(0)を切り替える
4. `delay`で指定した数値分ミリ秒単位で待機する

### 実行

プログラムはGitHub上にあるので、Roswellでインストールして実行してみて下さい。

```
ros install fireflower0/cl-raspi
```

REPLを起動し、以下のプログラムを実行します。

```
(ql:quickload :cl-raspi)
(cl-raspi/src/blink:main)
```

## I2C 温度センサー

I2Cとは、Inter Integrated Circuit の略で、「I2C」と書いて アイ・スクウェア・シー と呼びます。
フィリップス社で開発されたシリアルバスで、1980年代初期に提唱されました。
シリアルデータ (SDA) とシリアルクロック (SCL) の２本の信号線で情報伝達を行います。

### 使用した電子部品と回路図

電子部品は次のものを使用しました。

- ADT7410を使用した温度センサーモジュール  
販売サイト：http://akizukidenshi.com/catalog/g/gM-06675/

![ADT7410](images/09-pic-adt7410.jpg)

上記電子部品を以下のようにブレッドボードに配置します。

![回路図](images/09-circuit-diagram-adt7410.jpg)

### プログラム本体作成

プログラム本体を`src`ディレクトリ内に`i2c-temperature-sensor.lisp`という名前で作成します。

```
(defpackage :cl-raspi/src/i2c-temperature-sensor
  (:use :cl
        :cl-raspi/lib-wiring-pi)
  (:export :main))
(in-package :cl-raspi/src/i2c-temperature-sensor)

(defconstant +i2c-addr+ #X48)

(defun byte-swap (num-value)
  (let* ((str-value  (write-to-string num-value :base 16))
         (temp-msb   (subseq str-value 0 2))
         (temp-lsb   (subseq str-value 2)))
    (parse-integer (concatenate 'string temp-lsb temp-msb)
                   :radix 16)))

(defun get-data (fd)
  (* (byte-swap (wiringpi-i2c-read-reg16 fd #X00)) 0.0078))

(defun main ()
  (let ((fd (wiringpi-i2c-setup +i2c-addr+)))
    (wiringpi-i2c-write-reg8 fd #X03 #X80)
    (format t "~d~%" (get-data fd))))
```

最初に`cl-raspi/src/i2c-temperature-sensor`という名前でパッケージを定義し、`cl-raspi/lib-wiring-pi`を読み込みます。
また、外部から`main`関数を参照できるように`export`しておきます。
ADT7410のアドレスを`defconstant`で定数定義します(#X48)。  
I2Cデバイスのアドレスは、Raspberry Piと接続後、以下のコマンドを実行すると取得できます。

```
sudo i2cdetect -y 1
```

プログラム本体は`main`関数として書いていきます。流れとしては、以下の通りです。

1. `wiringpi-i2c-setup`でI2Cシステムの初期化
2. `wiringpi-i2c-write-reg8`でレジスタ`0x03`に`0x80`を書き込むことで、16ビットの高精度で温度を取得
3. `get-data`関数で温度データを取得し、コンソールに出力

- `get-data`関数  
`wiringpi-i2c-read-reg16`でレジスタ`0x00`から16ビットのデータを取得

- `byte-swap`関数  
温度データを取得するとビッグエンディアンになってしまっているので、バイトスワップしてリトルエンディアンに変換

### 温度データの計算について

- 13ビットの場合

4～16ビット目までが有効なデータなので、取得データを8で割って下位3ビットを捨ててから、温度分解能値である0.0625をかけます。  
計算式：(取得データ / 8) × 0.0625

- 16ビットの場合(今回はこっちを使用)

全てのデータが使えるので、そのまま温度分解能値である0.0078とかけます。  
計算式：取得データ × 0.0078

### 実行

プログラムはGitHub上にあるので、Roswellでインストールして実行してみて下さい。

```
ros install fireflower0/cl-raspi
```

REPLを起動し、以下のプログラムを実行します。

```
(ql:quickload :cl-raspi)
(cl-raspi/src/i2c-temperature-sensor:main)
```

## 実践：温度計付きデジタル時計

実践として温度計付きデジタル時計を作ってみましょう。

### 使用した電子部品と回路図

- 16桁×2行英数カナ表示液晶モジュール【MI2CLCD-01】  
搭載液晶：BO1602DGRNJB  
値段：1000円  
URL：https://www.marutsu.co.jp/pc/i/137795/

- ADT7410を使用した温度センサーモジュール  
値段：500円  
URL：http://akizukidenshi.com/catalog/g/gM-06675/

![回路図](images/09-circuit-diagram-simple-temperature.png)

### プログラム

一行目に日時、二行目に温度をLCDに表示するプログラムです。  
LCDの制御を入れるとコードが長くなるので`cl-raspi/bo1602dgrnjb`としてまとめました。

```
(defpackage :cl-raspi/src/simple-temperature
  (:use :cl
        :cl-raspi/lib-wiring-pi
        :cl-raspi/bo1602dgrnjb)
  (:export :main))
(in-package :cl-raspi/src/simple-temperature)

;; I2C device address (0x48)
(defconstant +i2c-addr-adt7410+ #X48)

(defun byte-swap (num-value)
  (let* ((str-value  (write-to-string num-value :base 16))
         (temp-msb   (subseq str-value 0 2))
         (temp-lsb   (subseq str-value 2)))
    (parse-integer (concatenate 'string temp-lsb temp-msb)
                   :radix 16)))

(defun get-data (fd)
  (* (byte-swap (wiringpi-i2c-read-reg16 fd #X00)) 0.0078))

(defun get-date ()
  (multiple-value-bind (second minute hour date month)
      (decode-universal-time (get-universal-time))
    (format nil "~A/~A ~A:~A:~A" month date hour minute second)))

(defun main ()
  (let ((adt7410-fd (wiringpi-i2c-setup +i2c-addr-adt7410+)))
    (bo1602dgrnjb-init)
    (wiringpi-i2c-write-reg8 adt7410-fd #X03 #X80)
    (loop
      (bo1602dgrnjb-text 1 (get-date))
      (bo1602dgrnjb-text 2 (format nil "temp:~,2f" (get-data adt7410-fd)))
      (delay 1000))))
```

### 実行

プログラムはGitHub上にあるので、Roswellでインストールして実行してみて下さい。

```
ros install fireflower0/cl-raspi
```

REPLを起動し、以下のプログラムを実行します。

```
(ql:quickload :cl-raspi)
(cl-raspi/src/simple-temperature:main)
```

![実行中の様子](images/09-simple-temperature-pic.png)

## 終わりに

ここまで読んでくださってありがとうございます。
Raspberry Pi での電子工作では Python が主流となっているようですが、Common Lisp でもできるよという話でした。
ラッパーを用意しなければいけないため少々面倒ではありますが、必要なものをその都度調べて使うため機能を理解できるし、一度作ってしまえば後は使いまわせるので気にするほど手間ではないと思っています。
簡単なことしか書けてないですが、基本的なことは一通り書けたと思います。
これを読んでCommon LispでRaspberry Pi電子工作をエンジョイして頂ければ幸いです。
