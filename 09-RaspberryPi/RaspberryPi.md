# Raspberry PiでCommon Lispを使おう

# はじめに

Raspberry Piで電子工作と言えば、Pythonで紹介している本や記事がとても多いです。  
しかし、自分はLisperなのでCommon Lispを使ってこれをやっていきます。  

# 環境の構築

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

起動したときの初期画面は以下のようになります。  

![lem-pic-001]()

この状態で、`M-x start-lisp-repl`コマンドを実行すると`REPL(Read-Eval-Print Loop)`が起動します。  

![lem-pic-002]()

基本的にここでプログラムを実行していきます。  

![lem-pic-003]()

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

プロジェクト用ディレクトリ構成は以下のようにします。

```
cl-raspi
  ├─ cl-raspi.asd
  ├─ lib-wiring-pi.lisp
  └─ src
       └─ ...
```

(※) Githubアカウントを持っているのであれば、ユーザー名を使うと良いです。  

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
  (:export :wiringpi-setup-gpio
           :pin-mode
           :digital-write
           :delay))
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

`lib-wiring-pi.lisp`に以下のプログラムを追記します。

```common-lisp
;; Init wiringPi
(defcfun ("wiringPiSetupGpio" wiringpi-setup-gpio) :int)

;; GPIO pin mode setting
(defcfun ("pinMode" pin-mode) :void
  (pin :int) (mode :int))

;; Output control of GPIO pin
(defcfun ("digitalWrite" digital-write) :void
  (pin :int) (value :int))

;; Delay (millisecond)
(defcfun ("delay" delay) :void
  (howlong :uint))
```

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

## タクトスイッチでGPIO入力

## シリアル通信

## I2C 温度センサー

## SPI 3軸加速度センサー

## I2C LCD

## OLED
