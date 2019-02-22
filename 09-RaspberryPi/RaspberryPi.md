# Raspberry PiでCommon Lispを使おう

# はじめに

Raspberry Piで電子工作と言えば、Pythonで紹介している本や記事がとても多いです。  
しかし、自分はLisperなのでCommon Lispを使ってこれをやっていきます。  

# 環境構築

ハードは`Raspberry Pi 3`、OSは`Raspbian Stretch`を使用します。  
`Raspbian Stretch`は以下のミラーサイトを使うと公式サイトよりも早くダウンロード出来ます。  
今回は執筆時での最新版`raspbian-2018-11-15`を使用しています。  

[http://ftp.jaist.ac.jp/pub/raspberrypi/raspbian/images/](http://ftp.jaist.ac.jp/pub/raspberrypi/raspbian/images/)

## Roswellのインストール


まずは、Roswellをインストールするために必要なものをインストールします。  
インストールするのは以下の3つです。  

- autoconf
- automake
- libcurl4-openssl-dev

```
sudo apt install autoconf automake libcurl4-openssl-dev
```

GitHubからRoswellのソースコードをダウンロードします。

```
git clone -b release https://github.com/roswell/roswell.git
```

ダウンロードしたRoswellのディレクトリへ移動します。

```
cd roswell
```

以下のコマンドを順に実行し、インストールを行います。

```
./bootstrap
./configure
make
sudo make install
```

Roswellのバージョンを確認

```
ros --version
```

執筆時での最新バージョンは`19.1.10.96`でした。

インストールが終わったら以下のコマンドで初期設定を行います。

```
ros setup
```

## Common Lispをインストール

今回はCommon Lisp処理系の1つである`CCL (Clozure Common Lisp)`を使います。

```
ros install ccl-bin
```

以下のコマンドでバージョンを確認します。

```
ros run -- --version
```

執筆時のバージョンは`Version 1.11/v1.11.5 (LinuxARM32)`でした。

## Lemのインストール

Common Lispで作成されたエディタである`Lem`をインストールします。  
`ncurses`が必要なので、最初にインストールしておきます。

```
sudo apt install libncurses5-dev libncursesw5-dev
```

RoswellでLemをインストールします。

```
ros install cxxxr/lem
```

環境変数を登録し、bash設定を再読み込みします。

```
echo export PATH=$PATH:~/.roswell/bin >> ~/.bashrc
source ~/.bashrc
```

ついでに、`sudo`時でもLemを使えるように設定します。  
以下のコマンドで`sudoers.tmp`ファイルを開き編集します。

```
sudo visudo
```

`env_reset`と`secure_path`の設定をコメントアウトし、PATHを保持する設定を追記します。

```
Defaults env_keep += "PATH"
```

以下のようになっていれば良いです。  

![visudo](https://github.com/clfreaks/techbookfest6/blob/master/09-RaspberryPi/pic/visudo.png)

以下のコマンドでバージョンを確認します。

```
lem --version
```

執筆時のバージョンは`lem 1.5 (ARM-raspberrypi)`でした。  

起動する時は、以下のコマンドを実行します。

```
lem --frontend ncurses-ccl
```

毎回これを打つのは面倒なので、エイリアスを設定し`lem`だけで起動できるようにします。  
bash設定の再読み込みも忘れずに。

```
echo alias lem=\'lem --frontend ncurses-ccl\'  >> ~/.bashrc
source ~/.bashrc
```

ついでに、sudo時でもエイリアスを使えるようにするために以下の設定も追加します。

```
echo alias sudo=\'sudo \' >> ~/.bashrc
source ~/.bashrc
```

起動したときの初期画面は以下のようになります。  

<img src="https://github.com/clfreaks/techbookfest6/blob/master/09-RaspberryPi/pic/lem-pic-001.png" width="320px">

この状態で、`M-x start-lisp-repl`コマンドを実行すると`REPL(Read-Eval-Print Loop)`が起動します。  

<img src="https://github.com/clfreaks/techbookfest6/blob/master/09-RaspberryPi/pic/lem-pic-002.png" width="320px">

基本的にここでプログラムを実行していきます。  

<img src="https://github.com/clfreaks/techbookfest6/blob/master/09-RaspberryPi/pic/lem-pic-003.png" width="320px">

## GPIO制御ライブラリについて

GPIO制御ライブラリとして`Wiring Pi`を使用します。  
`Raspbian Stretch`には最初からインストールされています。  

公式サイト：[http://wiringpi.com/](http://wiringpi.com/)  

ラッパーを作成しCommon Lispから呼び出して使用します。

# 電子工作してみよう

まずは、プロジェクト用ディレクトリを作成します。

```
cd ~/.roswell/local-projects
mkdir my-dir (※)
cd my-dir
mkdir cl-raspi
```

(※) Githubアカウントを持っているのであれば、ユーザー名を使うと良いです。  

プロジェクト用ディレクトリ構成は以下のようにします。

```
cl-raspi
  ├─ cl-raspi.asd
  ├─ lib-wiring-pi.lisp
  └─ src
       └─ ...
```

`cl-raspi.asd`の中身は以下のようにします。

```common-lisp
(defsystem "cl-raspi"
    :class :package-inferred-system
    :version "0.1.0"
    :license "MIT"
    :depends-on (;; 使用するライブラリ(今回はCFFI)
                 "cffi"
                 ;; ラッパー
                 "cl-raspi/lib-wiring-pi"
                 ;; プログラムを作成したら以下に追加していく
                 "cl-raspi/src/..."))
```

`lib-wiring-pi.lisp`はラッパーです。  
ここにWiringPiの関数を追加していきます。

```common-lisp
(defpackage :cl-raspi/lib-wiring-pi
  (:use :cl
        :cffi)
  (:export ;; ここに他のパッケージから参照したい定義名を追加していく
           ))
(in-package :cl-raspi/lib-wiring-pi)

(define-foreign-library libwiringPi
    (:unix "libwiringPi.so"))

(use-foreign-library libwiringPi)
```

CFFIとは、Common Lispから外部機能を利用するためのインターフェースです。
以下のような形式で記述していきます。

```
(defcfun ("WiringPiの機能" CommonLispで使うときの名前) :返り値
  (引数1 :データ型) (引数2 :データ型))
```

- [公式サイト(https://common-lisp.net/project/cffi/)](https://common-lisp.net/project/cffi/)
- [ユーザーマニュアル(https://common-lisp.net/project/cffi/manual/index.html)](https://common-lisp.net/project/cffi/manual/index.html)

最後に、REPLで以下のコマンドを実行するとプロジェクトが登録されます。  

```
(ql:register-local-projects)
```

## Lチカ

### 使用するWiringPi関数

Lチカで必要になるWiringPiの機能を`lib-wiring-pi.lisp`に追加していきます。

- wiringPiSetupGpio  
wiringPiの初期化に使用。  
エラーの場合は-1が返ってきます。  

```common-lisp
(defcfun ("wiringPiSetupGpio" wiringpi-setup-gpio) :int)
```

- pinMode  
GPIOピンのモード設定を行います。  
第1引数にGPIOピン番号、第2引数にモード(0：Input、1：Output)を設定。  

```common-lisp
(defcfun ("pinMode" pin-mode) :void
  (pin :int) (mode :int))

;; モード用定数
(defconstant +input+  0)
(defconstant +output+ 1)
```

- digitalWrite  
GPIOピンの出力制御を行います。  
第1引数にGPIOピン番号、第2引数に値(0 or 1)を設定。  

```common-lisp
(defcfun ("digitalWrite" digital-write) :void
  (pin :int) (value :int))
```

- delay  
待機処理を行います。  
引数で指定した値(ミリ秒)分待機します。  

```common-lisp
(defcfun ("delay" delay) :void
  (howlong :uint))
```

コードを追加したら、他のパッケージから参照出来るように、`export`に以下を追加して下さい。

```common-lisp
:+output+
:wiringpi-setup-gpio
:pin-mode
:digital-write
:delay
```

最終的に`lib-wiring-pi.lisp`は以下のようになっているはずです。

```common-lisp
(defpackage :cl-raspi/lib-wiring-pi
  (:use :cl
        :cffi)
  (:export :+output+
           :wiringpi-setup-gpio
           :pin-mode
           :digital-write
           :delay))
(in-package :cl-raspi/lib-wiring-pi)

(define-foreign-library libwiringPi
    (:unix "libwiringPi.so"))

(use-foreign-library libwiringPi)

(defconstant +input+ 0)
(defconstant +output+ 1)

(defcfun ("wiringPiSetupGpio" wiringpi-setup-gpio) :int)

(defcfun ("pinMode" pin-mode) :void
  (pin :int) (mode :int))

(defcfun ("digitalWrite" digital-write) :void
  (pin :int) (value :int))

(defcfun ("delay" delay) :void
  (howlong :uint))
```

### プログラム本体作成

プログラム本体を`src`ディレクトリ内に`blink.lisp`という名前で作成します。

```common-lisp
(defpackage :cl-raspi/src/blink
  (:use :cl
        :cl-raspi/lib-wiring-pi)
  (:export :main))
(in-package :cl-raspi/src/blink)

(defconstant +pin+ 11)

(defun main ()
  (wiringpi-setup-gpio)
  (pin-mode +pin+ 1)

  (loop
     (digital-write +pin+ 1)   ; Turn on LED
     (delay 500)               ; Delay 500(ms)
     (digital-write +pin+ 0)   ; Turn off LED
     (delay 500)))             ; Delay 500(ms)
```

### 回路図

電子部品は以下を使用しました。

- 赤色LED 1個
- 330Ω抵抗(橙橙茶金) 1個

<img src="https://github.com/clfreaks/techbookfest6/blob/master/09-RaspberryPi/CircuitDiagram/blink.jpg" width="320px">

### 実行

`cl-raspi`を`quicklisp`でロードし`cl-raspi/src/blink`パッケージの`main`関数を実行します。

```common-lisp
(ql:quickload :cl-raspi)
(cl-raspi/src/blink:main)
```

## タクトスイッチでGPIO入力

### 使用するWiringPi関数

前回作った物に必要な関数を足していきます。  
以下の2つを追加。

- pullUpDnControl  
端子に何も接続されていない場合の状態を設定するのに使用。  
3.3Vの場合は"2"、0Vの場合は"1"と設定する。

- digitalRead  
GPIO端子の状態を読み込む。  
ボタンを押すと"0"、放すと"1"になる。

### ラッパー作成

```common-lisp
(defpackage :cl-raspi/lib-wiring-pi
  (:use :cl
        :cffi)
  (:export :+input+
           :+output+
           :+pud-off+
           :+pud-down+
           :+pud-up+
           :wiringpi-setup-gpio
           :pin-mode
           :digital-read
           :digital-write
           :pull-updn-control
           :delay))
(in-package :cl-raspi/lib-wiring-pi)

(define-foreign-library libwiringPi
    (:unix "libwiringPi.so"))

(use-foreign-library libwiringPi)

;;; Constant

;; Pin mode
(defconstant +input+  0)
(defconstant +output+ 1)

;; Pull up/down/none
(defconstant +pud-off+  0)
(defconstant +pud-down+ 1)
(defconstant +pud-up+   2)

;;;; API

;;; Core Library

;; Init wiringPi
(defcfun ("wiringPiSetupGpio" wiringpi-setup-gpio) :int)

;; GPIO pin mode setting
(defcfun ("pinMode" pin-mode) :void
  (pin :int) (mode :int))

;; Read the status of the GPIO pin
(defcfun ("digitalRead" digital-read) :int
  (pin :int))

;; Output control of GPIO pin
(defcfun ("digitalWrite" digital-write) :void
  (pin :int) (value :int))
  
;; Set the state when nothing is connected to the terminal
(defcfun ("pullUpDnControl" pull-updn-control) :void
  (pin :int) (pud :int))

;;; Other

;; Delay (millisecond)
(defcfun ("delay" delay) :void
  (howlong :uint))
```

### プログラム本体作成

```common-lisp
(defpackage :cl-raspi/src/gpio-input
  (:use :cl
        :cl-raspi/lib-wiring-pi)
  (:export :main))
(in-package :cl-raspi/src/gpio-input)

(defconstant +pin+ 17)

(defun main ()
  (wiringpi-setup-gpio)
  (pin-mode +pin+ +input+)
  (pull-updn-control +pin+ +pud-up+)
  (loop
     (if (= (digital-read +pin+) 0)
         (format t "Switch ON~%")
         (format t "Switch OFF~%"))
     (delay 500)))
```

### 回路図

電子部品は以下を使用しました。

- タクトスイッチ 1個
- 1kΩ抵抗(茶黒赤金) 1個

<img src="https://github.com/clfreaks/techbookfest6/blob/master/09-RaspberryPi/CircuitDiagram/gpio-input.jpg" width="320px">

### 実行

`cl-raspi`を`quicklisp`でロードし`cl-raspi/src/gpio-input`パッケージの`main`関数を実行します。

```common-lisp
(ql:quickload :cl-raspi)
(cl-raspi/src/gpio-input:main)
```

## PWM

### 使用するWiringPi関数

- pwm-set-mode  
PWMジェネレータは2つのモード(バランス、マークスペース)で動作させることが出来ます。  
デフォルトはバランスモードです。  
`+pwm-mode-ms+`または`+pwm-mode-bal+`で切り替えます。

- pwm-set-range  
PWMジェネレータの範囲レジスタを設定します。  
デフォルトは1024です。

- pwm-set-clock  
PWMクロックの約数を設定します。

- pwm-write  
指定されたピンのPWMレジスタに値を書き込みます。  
aspberry Piには1つのオンボードPWMピン、ピン1（BMC_GPIO 18、Phys 12）があり、範囲は0〜1024です。

### ラッパー作成

```common-lisp
(defpackage :cl-raspi/lib-wiring-pi
  (:use :cl
        :cffi)
  (:export :+input+
           :+output+
           :+pud-off+
           :+pud-down+
           :+pud-up+
           :+pwm-output+
           :+pwm-mode-ms+
           :+pwm-mode-bal+
           :wiringpi-setup-gpio
           :pin-mode
           :digital-read
           :digital-write
           :pull-updn-control
           :pwm-set-mode
           :pwm-set-range
           :pwm-set-clock
           :pwm-write
           :delay))
(in-package :cl-raspi/lib-wiring-pi)

(define-foreign-library libwiringPi
    (:unix "libwiringPi.so"))

(use-foreign-library libwiringPi)

;;; Constant

;; Pin mode
(defconstant +input+      0)
(defconstant +output+     1)
(defconstant +pwm-output+ 2)

;; PWM
(defconstant +pwm-mode-ms+  0)
(defconstant +pwm-mode-bal+ 1)

;; Pull up/down/none
(defconstant +pud-off+  0)
(defconstant +pud-down+ 1)
(defconstant +pud-up+   2)

;;;; API

;;; Core Library

;; Init wiringPi
(defcfun ("wiringPiSetupGpio" wiringpi-setup-gpio) :int)

;; GPIO pin mode setting
(defcfun ("pinMode" pin-mode) :void
  (pin :int) (mode :int))

;; Read the status of the GPIO pin
(defcfun ("digitalRead" digital-read) :int
  (pin :int))

;; Output control of GPIO pin
(defcfun ("digitalWrite" digital-write) :void
  (pin :int) (value :int))
  
;; Set the state when nothing is connected to the terminal
(defcfun ("pullUpDnControl" pull-updn-control) :void
  (pin :int) (pud :int))

;;; PWM Library

;; PWM set mode
(defcfun ("pwmSetMode" pwm-set-mode) :void
  (mode :int))

;; PWM set range (default 1024)
(defcfun ("pwmSetRange" pwm-set-range) :void
  (range :uint))

;; PWM set clock
(defcfun ("pwmSetClock" pwm-set-clock) :void
  (divisor :int))

;; PWM write
(defcfun ("pwmWrite" pwm-write) :void
  (pin :int) (value :int))

;;; Other

;; Delay (millisecond)
(defcfun ("delay" delay) :void
  (howlong :uint))
```

### プログラム本体作成

```common-lisp
(defpackage :cl-raspi/src/servomotor
  (:use :cl
        :cl-raspi/lib-wiring-pi)
  (:export :main))
(in-package :cl-raspi/src/servomotor)

(defconstant +pin+ 12)

(defun init ()
  (wiringpi-setup-gpio)
  (pin-mode +pin+ +pwm-output+)
  (pwm-set-mode +pwm-mode-ms+)
  (pwm-set-range 1024)
  (pwm-set-clock 375))

(defun main ()
  (init)
  (let ((move 0))
    (loop
      (setf move (read))
      (pwm-write +pin+ move))))
```

### 回路図

電子部品は以下を使用しました。

- マイクロサーボ9g SG-90  
[http://akizukidenshi.com/catalog/g/gM-08761/](http://akizukidenshi.com/catalog/g/gM-08761/)

<img src="https://github.com/clfreaks/techbookfest6/blob/master/09-RaspberryPi/CircuitDiagram/servomotor.jpg" width="320px">

### 実行

ハードウェアPWMはrootユーザーの権限でコマンドを実行する必要があります。  
なので、Lemを起動する時は`sudo lem`としてください。

`cl-raspi`を`quicklisp`でロードし`cl-raspi/src/servomotor`パッケージの`main`関数を実行します。

```common-lisp
(ql:quickload :cl-raspi)
(cl-raspi/src/servomotor:main)
```

## I2C 温度センサー

### 使用するWiringPi関数

- wiringPiI2CSetup  
これにより、指定されたデバイスIDでI2Cシステムが初期化されます。  
IDはデバイスのI2C番号で、これを見つけるためにi2cdetectコマンドを使用できます。  
戻り値は標準のLinuxファイルハンドルで、エラーがあれば-1を返します。

- wiringPiI2CWriteReg8  
8ビットのデータ値を指示されたデバイスレジスタに書き込みます。

- wiringPiI2CReadReg16  
示されたデバイス・レジスタから16ビットの値を読み出します。

### ラッパー作成

```common-lisp
(defpackage :cl-raspi/lib-wiring-pi
  (:use :cl
        :cffi)
  (:export :+input+
           :+output+
           :+pud-off+
           :+pud-down+
           :+pud-up+
           :+pwm-output+
           :+pwm-mode-ms+
           :+pwm-mode-bal+
           :wiringpi-setup-gpio
           :pin-mode
           :digital-read
           :digital-write
           :pull-updn-control
           :pwm-set-mode
           :pwm-set-range
           :pwm-set-clock
           :pwm-write
           :wiringpi-i2c-setup
           :wiringpi-i2c-write-reg8
           :wiringpi-i2c-read-reg16
           :delay))
(in-package :cl-raspi/lib-wiring-pi)

(define-foreign-library libwiringPi
    (:unix "libwiringPi.so"))

(use-foreign-library libwiringPi)

;;; Constant

;; Pin mode
(defconstant +input+      0)
(defconstant +output+     1)
(defconstant +pwm-output+ 2)

;; PWM
(defconstant +pwm-mode-ms+  0)
(defconstant +pwm-mode-bal+ 1)

;; Pull up/down/none
(defconstant +pud-off+  0)
(defconstant +pud-down+ 1)
(defconstant +pud-up+   2)

;;;; API

;;; Core Library

;; Init wiringPi
(defcfun ("wiringPiSetupGpio" wiringpi-setup-gpio) :int)

;; GPIO pin mode setting
(defcfun ("pinMode" pin-mode) :void
  (pin :int) (mode :int))

;; Read the status of the GPIO pin
(defcfun ("digitalRead" digital-read) :int
  (pin :int))

;; Output control of GPIO pin
(defcfun ("digitalWrite" digital-write) :void
  (pin :int) (value :int))
  
;; Set the state when nothing is connected to the terminal
(defcfun ("pullUpDnControl" pull-updn-control) :void
  (pin :int) (pud :int))

;;; PWM Library

;; PWM set mode
(defcfun ("pwmSetMode" pwm-set-mode) :void
  (mode :int))

;; PWM set range (default 1024)
(defcfun ("pwmSetRange" pwm-set-range) :void
  (range :uint))

;; PWM set clock
(defcfun ("pwmSetClock" pwm-set-clock) :void
  (divisor :int))

;; PWM write
(defcfun ("pwmWrite" pwm-write) :void
  (pin :int) (value :int))

;;; I2C Library

;; Initialization of the I2C systems.
(defcfun ("wiringPiI2CSetup" wiringpi-i2c-setup) :int
  (fd :int))

;; Writes 8-bit data to the instructed device register.
(defcfun ("wiringPiI2CWriteReg8" wiringpi-i2c-write-reg8) :int
  (fd :int) (reg :int) (data :int))

;; It reads the 16-bit value from the indicated device register.
(defcfun ("wiringPiI2CReadReg16" wiringpi-i2c-read-reg16) :int
  (fd :int) (reg :int))

;;; Other

;; Delay (millisecond)
(defcfun ("delay" delay) :void
  (howlong :uint))
```

### プログラム本体作成

```common-lisp
(defpackage :cl-raspi/src/i2c-temperature-sensor
  (:use :cl
        :cl-raspi/lib-wiring-pi)
  (:export :main))
(in-package :cl-raspi/src/i2c-temperature-sensor)

;; I2C device address (0x48)
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

### 回路図

### 実行

## SPI 3軸加速度センサー

### 使用するWiringPi関数

### ラッパー作成

```common-lisp
(defpackage :cl-raspi/lib-wiring-pi
  (:use :cl
        :cffi)
  (:export :+input+
           :+output+
           :+pwm-output+
           :+pwm-mode-ms+
           :+pwm-mode-bal+
           :+pud-off+
           :+pud-down+
           :+pud-up+
           :wiringpi-setup-gpio
           :pin-mode
           :digital-read
           :digital-write
           :pull-updn-control
           :pwm-set-mode
           :pwm-set-range
           :pwm-set-clock
           :pwm-write
           :wiringpi-i2c-setup
           :wiringpi-i2c-write-reg8
           :wiringpi-i2c-read-reg16
           :wiringpi-spi-setup
           :wiringpi-spi-data-rw
           :delay))
(in-package :cl-raspi/lib-wiring-pi)

(define-foreign-library libwiringPi
    (:unix "libwiringPi.so"))

(use-foreign-library libwiringPi)

;;; Constant

;; Pin mode
(defconstant +input+      0)
(defconstant +output+     1)
(defconstant +pwm-output+ 2)

;; PWM
(defconstant +pwm-mode-ms+  0)
(defconstant +pwm-mode-bal+ 1)

;; Pull up/down/none
(defconstant +pud-off+  0)
(defconstant +pud-down+ 1)
(defconstant +pud-up+   2)

;;;; API

;;; Core Library

;; Init wiringPi
(defcfun ("wiringPiSetupGpio" wiringpi-setup-gpio) :int)

;; GPIO pin mode setting
(defcfun ("pinMode" pin-mode) :void
  (pin :int) (mode :int))

;; Read the status of the GPIO pin
(defcfun ("digitalRead" digital-read) :int
  (pin :int))

;; Output control of GPIO pin
(defcfun ("digitalWrite" digital-write) :void
  (pin :int) (value :int))

;; Set the state when nothing is connected to the terminal
(defcfun ("pullUpDnControl" pull-updn-control) :void
  (pin :int) (pud :int))

;;; PWM Library

;; PWM set mode
(defcfun ("pwmSetMode" pwm-set-mode) :void
  (mode :int))

;; PWM set range (default 1024)
(defcfun ("pwmSetRange" pwm-set-range) :void
  (range :uint))

;; PWM set clock
(defcfun ("pwmSetClock" pwm-set-clock) :void
  (divisor :int))

;; PWM write
(defcfun ("pwmWrite" pwm-write) :void
  (pin :int) (value :int))

;;; I2C Library

;; Initialization of the I2C systems.
(defcfun ("wiringPiI2CSetup" wiringpi-i2c-setup) :int
  (fd :int))

;; Writes 8-bit data to the instructed device register.
(defcfun ("wiringPiI2CWriteReg8" wiringpi-i2c-write-reg8) :int
  (fd :int) (reg :int) (data :int))

;; It reads the 16-bit value from the indicated device register.
(defcfun ("wiringPiI2CReadReg16" wiringpi-i2c-read-reg16) :int
  (fd :int) (reg :int))

;;; SPI Library

;; SPI initialization
(defcfun ("wiringPiSPISetup" wiringpi-spi-setup) :int
  (channel :int) (speed :int))

;; Execute concurrent write/read transactions on the selected SPI bus
(defcfun ("wiringPiSPIDataRW" wiringpi-spi-data-rw) :int
  (channel :int) (data :pointer) (len :int))

;;; Other

;; Delay (millisecond)
(defcfun ("delay" delay) :void
  (howlong :uint))
```

### プログラム本体作成

```common-lisp
(defpackage :cl-raspi/src/3-axis-acceleration-sensor
  (:use :cl
        :cl-raspi/lib-wiring-pi)
  (:import-from :cffi)
  (:export :main))
(in-package :cl-raspi/src/3-axis-acceleration-sensor)

(defconstant +spi-cs+ 0)                ; Select target SPI device
(defconstant +spi-speed+ 100000)        ; SPI communication speed

(defconstant +out-x-l+ #X28)            ; OUT_X Low
(defconstant +out-x-h+ #X29)            ; OUT_X High
(defconstant +out-y-l+ #X2A)            ; OUT_Y Low
(defconstant +out-y-h+ #X2B)            ; OUT_Y High
(defconstant +out-z-l+ #X2C)            ; OUT_Z Low
(defconstant +out-z-h+ #X2D)            ; OUT_Z High

(defconstant +who-am-i+  #X0F)          ; WHO_AM_I
(defconstant +ctrl-reg1+ #X20)          ; CTRL_REG1
(defconstant +ctrl-reg4+ #X23)          ; CTRL_REG4

(defconstant +read+  #X80)              ; Read
(defconstant +write+ #X3F)              ; Write

(defconstant +pin+   8)                 ; CS
(defconstant +high+  1)
(defconstant +low+   0)

(defun spi-data-rw (channel data &optional (len (length data)))
  (let ((mp (cffi:foreign-alloc :unsigned-char :count len :initial-contents data)))
    (digital-write +pin+ +low+)
    (wiringpi-spi-data-rw channel mp len)
    (digital-write +pin+ +high+)
    (let ((rval (loop for i from 0 below len
                   collect (cffi:mem-aref mp :unsigned-char i))))
      (cffi:foreign-free mp)
      rval)))

(defun spi-read (read-addr)
  (let (outdat out)
    (setf outdat (list (logior read-addr +read+) #X00))
    (setf out (spi-data-rw +spi-cs+ outdat))
    (nth 1 out)))

(defun spi-write (write-addr data)
  (spi-data-rw +spi-cs+ (list (logand write-addr +write+) data)))

(defun conv-two-byte (high low)
  (let (dat)
    (setq dat (logior (ash high 8) low))
    (if (>= high #X80)
        (setf dat (- dat 65536)))
    (setq dat (ash dat -4))
    dat))

(defun main ()
  (let (lb hb x y z)
    (wiringpi-spi-setup +spi-cs+ +spi-speed+)
    (wiringpi-setup-gpio)
    (pin-mode +pin+ +output+)
    (digital-write +pin+ +high+)

    (if (equal (spi-read +who-am-i+) #X33)
        (format t "I AM LIS3DH~%")
        (return-from main nil))
    (spi-write +ctrl-reg1+ #X77)

    (loop
       ;; Get X axis data
       (setf lb (spi-read +out-x-l+))
       (setf hb (spi-read +out-x-h+))
       (setf x  (conv-two-byte hb lb))

       ;; Get Y axis data
       (setf lb (spi-read +out-y-l+))
       (setf hb (spi-read +out-y-h+))
       (setf y  (conv-two-byte hb lb))

       ;; Get Z axis data
       (setf lb (spi-read +out-z-l+))
       (setf hb (spi-read +out-z-h+))
       (setf z  (conv-two-byte hb lb))

       (format t "x=~6d y=~6d z=~6d~%" x y z)

       (delay 500))))
```

### 回路図

### 実行

## I2C LCD

### 使用するWiringPi関数

### ラッパー作成

### プログラム本体作成

### 回路図

### 実行

## OLED

### 使用するWiringPi関数

### ラッパー作成

### プログラム本体作成

### 回路図

### 実行
