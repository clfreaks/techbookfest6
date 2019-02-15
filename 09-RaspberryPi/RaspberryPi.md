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
echo export PATH=$PATH:~/.roswell/bin >> ~/.bash_profile
source ~/.bash_profile
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
echo alias lem=\'lem --frontend ncurses-ccl\'  >> ~/.bash_profile
source ~/.bash_profile
```

ついでに、sudo時でもエイリアスを使えるようにするために以下の設定も追加します。

```
echo alias sudo=\'sudo \' >> ~/.bash_profile
source ~/.bash_profile
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
        :cffi))
(in-package :cl-raspi/lib-wiring-pi)

(define-foreign-library libwiringPi
    (:unix "libwiringPi.so"))

(use-foreign-library libwiringPi)
```

最後に、REPLで以下のコマンドを実行するとプロジェクトが登録されます。  

```
(ql:register-local-projects)
```

## Lチカ

### 使用するWiringPi関数

Lチカで必要になるWiringPiの関数は以下の4つです。

- wiringPiSetupGpio  
wiringPiの初期化に使用。  
エラーの場合は-1が返ってきます。  

- pinMode  
GPIOピンのモード設定を行います。  
第1引数にGPIOピン番号、第2引数にモード(0：Input、1：Output)を設定。  

- digitalWrite  
GPIOピンの出力制御を行います。  
第1引数にGPIOピン番号、第2引数に値(0 or 1)を設定。  

- delay  
待機処理を行います。  
引数で指定した値[ミリ秒)分待機します。  


### ラッパー作成

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

;;; Constant

;; Pin mode
(defconstant +output+ 1)

;;;; API

;;; Core Library

;; Init wiringPi
(defcfun ("wiringPiSetupGpio" wiringpi-setup-gpio) :int)

;; GPIO pin mode setting
(defcfun ("pinMode" pin-mode) :void
  (pin :int) (mode :int))

;; Output control of GPIO pin
(defcfun ("digitalWrite" digital-write) :void
  (pin :int) (value :int))

;;; Other

;; Delay (millisecond)
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

  ;; Infinite loop (Ctrl-c exits loop)
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

### ラッパー作成

### プログラム本体作成

### 回路図

### 実行

## SPI 3軸加速度センサー

### 使用するWiringPi関数

### ラッパー作成

### プログラム本体作成

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
