# Common LispでRaspberry Pi電子工作

## はじめに

Raspberry Pi は、電子工作もできる小型PCです。
主に教育現場や安価に入手できる IOT 機器として趣味や業務に使われています。
Raspberry Pi で電子工作と言えば、 Python を使って紹介している記事や本がとても多いですが、 Common Lisp でも同じようにすることができます。
Common Lisp で電子工作するメリットは、C言語ほどではないですがそこそこ実行速度が早いことや、 REPL で簡単に動作を確認できることなどがあります。
今回は、開発環境の構築方法と簡単な電子工作の例をいくつか挙げて Common Lisp を使った Raspberry Pi 電子工作について紹介したいと思います。

## 環境構築

ハードは`Raspberry Pi 3`、OSは`Raspbian Stretch`を使用します。
`Raspbian Stretch`は以下のミラーサイトを使うと公式サイトよりも早くダウンロード出来ます。

```
ミラーサイトURL：http://ftp.jaist.ac.jp/pub/raspberrypi/raspbian/images/
```

今回は執筆時での最新版`raspbian-2018-11-15`を使用しています。

## Roswellのインストール

Roswellは基本的に`homebrew (Linuxではlinuxbrew)`でインストールしますが、`homebrew`がRaspberry PiのCPUであるARM32をサポートしていないため、以下に示す手順でソースコードをビルドしてインストールします。まずは、Roswellをインストールするために必要なものをインストールします。インストールするのは以下の3つです。

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

ARM32の`SBCL(Steel Bank Common Lisp)`がスレッド対応していないため、今回は`CCL (Clozure Common Lisp)`を使用します。

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

![visudo](images/09-pic-visudo.png)

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

![lem1](images/09-pic-lem-001.png)

この状態で、`M-x start-lisp-repl`コマンドを実行すると`REPL(Read-Eval-Print Loop)`が起動します。  

![lem2](images/09-pic-lem-002.png)

基本的にここでプログラムを実行していきます。  

![lem3](images/09-pic-lem-003.png)

## 無限ループからの脱出方法について

電子工作では、無限ループ内でLEDを点滅させたり、スイッチの押下やセンサーの情報を待ち受けたり等で無限ループを使うことが多いです。  
LemのREPLを使っていて、無限ループプログラムを終了させたいときは、以下のコマンドを実行してください。

```
C-c C-c
```

または

```
C-c g
```

以下のような割り込みが発生して、プログラムが停止します。

```
Interrupt from Emacs
   [Condition of type SIMPLE-ERROR]
```

この状態で、`q`キーを押下するとREPLに戻り、無限ループから脱出できます。

## GPIO制御ライブラリについて

GPIO制御ライブラリとして`Wiring Pi`を使用します。
`Raspbian Stretch`には最初からインストールされています。
ラッパーを作成しCommon Lispから呼び出して使用します。

```
公式サイト：http://wiringpi.com/
```

## プロジェクトの作成

まずは、プロジェクト用ディレクトリを作成します。

```
cd ~/.roswell/local-projects
mkdir my-dir (※)
cd my-dir
mkdir cl-raspi
```

(※) GitHubアカウントを持っているのであれば、ユーザー名を使うと良いです。  

プロジェクト用ディレクトリ構成は以下のようにします。

```
cl-raspi
  ├─ cl-raspi.asd
  ├─ lib-wiring-pi.lisp
  └─ src
       └─ ...
```

`cl-raspi.asd`の中身は以下のようにします。

```
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

`lib-wiring-pi.lisp`はラッパーです。ここにWiringPiの関数を追加していきます。

```
(defpackage :cl-raspi/lib-wiring-pi
  (:use :cl
        :cffi)
  (:export ;; ここに他のパッケージから参照したい定義名を追加していく
           ))
(in-package :cl-raspi/lib-wiring-pi)

(define-foreign-library libwiringPi
    (:unix "libwiringPi.so"))

(use-foreign-library libwiringPi)

;; ここから下に WiringPi の関数を追加していきます。

```

CFFIとは、Common Lispから外部機能を利用するためのインターフェースです。
以下のような形式で記述していきます。

```
(defcfun ("WiringPiの機能" CommonLispで使うときの名前) :返り値のデータ型
  (引数1 :データ型) (引数2 :データ型) ...)
```

CFFIについての参考文献

```
- 公式サイト：https://common-lisp.net/project/cffi/
- ユーザーマニュアル：https://common-lisp.net/project/cffi/manual/index.html
```

最後に、REPLで以下のコマンドを実行するとプロジェクトが登録されます。

```
(ql:register-local-projects)
```

## LED

最初は電子工作の基本、LEDを点滅させるLチカをやってみます。

### 使用するWiringPi関数

最初に、必要になるWiringPiの機能を`lib-wiring-pi.lisp`に追加していきます。

- wiringPiSetupGpio  
wiringPiの初期化に使用。  
エラーの場合は-1が返ってきます。

```
(defcfun ("wiringPiSetupGpio" wiringpi-setup-gpio) :int)
```

- pinMode  
GPIOピンのモード設定を行います。  
第1引数にGPIOピン番号、第2引数にモード(0：Input、1：Output、2：PWM Output)を設定。

```
(defcfun ("pinMode" pin-mode) :void
  (pin :int) (mode :int))

;; モード用定数
(defconstant +input+  0)
(defconstant +output+ 1)
(defconstant +pwm-output+ 2)
```

- digitalWrite  
GPIOピンの出力制御を行います。  
第1引数にGPIOピン番号、第2引数に値(0 or 1)を設定。

```
(defcfun ("digitalWrite" digital-write) :void
  (pin :int) (value :int))
```

- delay  
待機処理を行います。  
引数で指定した値(ミリ秒)分待機します。

```
(defcfun ("delay" delay) :void
  (howlong :uint))
```

コードを追加したら、他のパッケージから参照出来るように、`export`に以下を追加して下さい。

```
:+input+
:+output+
:+pwm-output+
:wiringpi-setup-gpio
:pin-mode
:digital-write
:delay
```

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

`cl-raspi.asd`に作成したパッケージ`cl-raspi/src/blink`を追加します。

```
(defsystem "cl-raspi"
    :class :package-inferred-system
    :version "0.1.0"
    :license "MIT"
    :depends-on ("cffi"
                 "cl-raspi/lib-wiring-pi"
                 "cl-raspi/src/cl-raspi/src/blink"))
```

`cl-raspi`を`quicklisp`でロードし`cl-raspi/src/blink`パッケージの`main`関数を実行します。

```
(ql:quickload :cl-raspi)
(cl-raspi/src/blink:main)
```

## タクトスイッチ

Raspberry Pi に何かしら指示を与えたいときにはスイッチを使用します。  
今回は、ボタン形式のスイッチ「タクトスイッチ」の押下を Raspberry Pi 側で検知してみます。

### 使用するWiringPi関数

最初に、必要になるWiringPiの機能を`lib-wiring-pi.lisp`に追加していきます。
前回作った物に必要な関数を追加していきます。  

- pullUpDnControl  
GPIOピンに何も接続されていない場合の状態を設定します。  
第1引数にGPIOピン番号、第2引数にモード(0：PUD\_OFF、1：PUD\_DOWN、2：PUD_UP)を設定。

GPIOピンは本来、何も接続していない状態だと周りの電子的なノイズにより入力値が不安定になります。Raspberry Piでは「プルアップ(PUD\_UP)」または「プルダウン(PUD\_DOWN)」という回路を使ってGPIOピンの状態を安定させています。  
プルアップでは、入力端子に抵抗(50k〜65kΩ)を介して3.3Vに接続しておくことで、入力端子に何も接続場合に端子を3.3Vの状態で安定させます。プルダウンでは、GNDに接続しておき、0Vの状態にします。

```
(defcfun ("pullUpDnControl" pull-updn-control) :void
  (pin :int) (pud :int))

;; モード用定数
(defconstant +pud-off+  0)
(defconstant +pud-down+ 1)
(defconstant +pud-up+   2)
```

- digitalRead  
指定したピンの状態を読んでHIGH(1) またはLOW(0) の値を返します。

```
(defcfun ("digitalRead" digital-read) :int
  (pin :int))
```

コードを追加したら、他のパッケージから参照出来るように、`export`に以下を追加して下さい。

```
:+pud-off+
:+pud-down+
:+pud-up+
:digital-read
:pull-updn-control
```

### 使用した電子部品と回路図

電子部品は次のものを使用しました。

- タクトスイッチ 1個
- 1kΩ抵抗(茶黒赤金) 1個

上記電子部品を以下のようにブレッドボードに配置します。

![回路図](images/09-circuit-diagram-gpio-input.jpg)

### プログラム本体作成

プログラム本体を`src`ディレクトリ内に`button.lisp`という名前で作成します。

```
(defpackage :cl-raspi/src/button
  (:use :cl
        :cl-raspi/lib-wiring-pi)
  (:export :main))
(in-package :cl-raspi/src/button)

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

最初に`cl-raspi/src/button`という名前でパッケージを定義し、`cl-raspi/lib-wiring-pi`を読み込みます。
また、外部から`main`関数を参照できるように`export`しておきます。
タクトスイッチをつないだGPIOピン番号を`defconstant`で定数定義します(今回はGPIO17を使用)。
プログラム本体は`main`関数として書いていきます。
流れとしては、以下の通りです。

1. `wiringpi-setup-gpio`で初期化
2. `pin-mode`でGPIO17を入力モードに設定
3. `pull-updn-control`でGPIO17ピンを`PUD_UP`モードに設定
4. 無限ループ内でタクトスイッチ押下を待ち受ける
5. タクトスイッチが押下されるとピンの状態がLOW(0)になり、離すとHIGH(1)
6. 状態によってコンソールに異なる文字列を出力

### 実行

`cl-raspi.asd`に作成したパッケージ`cl-raspi/src/button`を追加します。

```
(defsystem "cl-raspi"
    :class :package-inferred-system
    :version "0.1.0"
    :license "MIT"
    :depends-on ("cffi"
                 "cl-raspi/lib-wiring-pi"
                 "cl-raspi/src/button"))
```

`cl-raspi`を`quicklisp`でロードし`cl-raspi/src/gpio-input`パッケージの`main`関数を実行します。

```
(ql:quickload :cl-raspi)
(cl-raspi/src/button:main)
```

## ソフトウェアPWM

PWM(Pulse Width Modulation)とは、電力を制御する方式の1つで、オンとオフを繰り返し切り替えて出力される電圧を制御することができます。  
今回はPWMをつかってRGBフルカラーLED光らせてみます。

### 使用するWiringPi関数

ソフトウェアPWMで必要になるWiringPiの機能を`lib-wiring-pi.lisp`に追加していきます。

- soft-pwm-create  
ソフトウェア制御のPWMピンを生成。  
第1引数に任意のGPIOピン番号、第2引数にPWMの初期値、第3引数にPWMの最大値を指定します。

```
(defcfun ("softPwmCreate" soft-pwm-create) :int
  (pin :int) (initial-value :int) (pwm-range :int))
```

- soft-pwm-write  
第1引数で指定されたGPIOピン番号を第2引数の値でPWMを更新。  
値が範囲内であることが確認され、`soft-pwm-create`によって初期化されていないGPIOピンの場合は無視される。

```
(defcfun ("softPwmWrite" soft-pwm-write) :void
  (pin :int) (value :int))
```

### 使用した電子部品と回路図

- RGBフルカラーLED カソードコモン (OSTA5131A)
- トランジスタ (2SC1815GR) 3個
- 120 Ω抵抗 (茶赤茶金)
- 150 Ω抵抗 (茶緑茶金) 3個
- 10k Ω抵抗 (茶黒橙金) 3個

上記電子部品を以下のようにブレッドボードに配置します。

![回路図](images/09-circuit-diagram-color.jpg)

### プログラム本体作成

プログラム本体を`src`ディレクトリ内に`color.lisp`という名前で作成します。

```
(defpackage :cl-raspi/src/color
  (:use :cl
        :cl-raspi/lib-wiring-pi)
  (:export :main))
(in-package :cl-raspi/src/color)

(defconstant +green-pin+ 18)
(defconstant +blue-pin+  23)
(defconstant +red-pin+   24)

(defun main ()
  ;; GPIO初期化
  (wiringpi-setup-gpio)
  ;; ピンモード設定
  (pin-mode +green-pin+ +output+)
  (pin-mode +blue-pin+  +output+)
  (pin-mode +red-pin+   +output+)
  ;; PWM出力設定
  (soft-pwm-create +green-pin+ 0 100)
  (soft-pwm-create +blue-pin+  0 100)
  (soft-pwm-create +red-pin+   0 100)
  ;; PWMで各端子に出力
  (soft-pwm-write +green-pin+ 30)
  (soft-pwm-write +blue-pin+  50)
  (soft-pwm-write +red-pin+  100))
```

最初に`cl-raspi/src/color`という名前でパッケージを定義し、`cl-raspi/lib-wiring-pi`を読み込みます。
また、外部から`main`関数を参照できるように`export`しておきます。
RGBフルカラーLEDをつないだGPIOピン番号を`defconstant`で定数定義します(今回はGPIO18/GPIO23/GPIO24を使用)。
プログラム本体は`main`関数として書いていきます。流れとしては、以下の通りです。

1. `wiringpi-setup-gpio`で初期化
2. `pin-mode`でGPIO18/GPIO23/GPIO24を出力モードに設定
3. `soft-pwm-create`でPWMの指定する数値の範囲を設定(今回は0〜100の間で制御するように設定)
4. `soft-pwm-write`で各GPIOピンにPWMで出力する割合を指定

### 実行

`cl-raspi.asd`に作成したパッケージ`cl-raspi/src/color`を追加します。

```
(defsystem "cl-raspi"
    :class :package-inferred-system
    :version "0.1.0"
    :license "MIT"
    :depends-on ("cffi"
                 "cl-raspi/lib-wiring-pi"
                 "cl-raspi/src/color"))
```

`cl-raspi`を`quicklisp`でロードし`cl-raspi/src/color`パッケージの`main`関数を実行します。

```
(ql:quickload :cl-raspi)
(cl-raspi/src/color:main)
```

## ハードウェアPWM

ハードウェアPWMを使用してサーボモーターを制御してみます。

### 使用するWiringPi関数

ハードウェアPWMで必要になるWiringPiの機能を`lib-wiring-pi.lisp`に追加していきます。

- pwm-set-mode  
PWMジェネレータは2つのモード(バランス、マークスペース)で動作させることが出来ます。  
デフォルトはバランスモードです。  
`+pwm-mode-ms+`または`+pwm-mode-bal+`で切り替えます。

```
(defcfun ("pwmSetMode" pwm-set-mode) :void
  (mode :int))

;; モード用定数
(defconstant +pwm-mode-ms+  0)
(defconstant +pwm-mode-bal+ 1)
```

- pwm-set-range  
PWMジェネレータの範囲レジスタを設定します。  
デフォルトは1024です。

```
(defcfun ("pwmSetRange" pwm-set-range) :void
  (range :uint))
```

- pwm-set-clock  
PWMクロックの約数を設定します。

```
(defcfun ("pwmSetClock" pwm-set-clock) :void
  (divisor :int))
```

- pwm-write  
指定されたピンのPWMレジスタに値を書き込みます。  
aspberry Piには1つのオンボードPWMピン、ピン1（BMC_GPIO 18、Phys 12）があり、範囲は0〜1024です。

```
(defcfun ("pwmWrite" pwm-write) :void
  (pin :int) (value :int))
```

コードを追加したら、他のパッケージから参照出来るように、`export`に以下を追加して下さい。

```
:+pwm-mode-ms+
:+pwm-mode-bal+
:pwm-set-mode
:pwm-set-range
:pwm-set-clock
:pwm-write
```

### 使用した電子部品と回路図

電子部品は次のものを使用しました。

- マイクロサーボ9g SG-90  
販売サイト：http://akizukidenshi.com/catalog/g/gM-08761/

上記電子部品を以下のようにブレッドボードに配置します。

![回路図](images/09-circuit-diagram-servomotor.jpg)

### プログラム本体作成

プログラム本体を`src`ディレクトリ内に`servomotor.lisp`という名前で作成します。

```
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
  (let ((set-degree 0) 
        (move-deg 0)) 
    (loop 
      (setf set-degree (read)) 
      (when (and (<= set-degree 90) (>= set-degree -90))
        (setf move-deg (floor (+ 74 (* (/ 48 90) set-degree))))
        (pwm-write +pin+ move-deg))))) 
```

最初に`cl-raspi/src/servomotor`という名前でパッケージを定義し、`cl-raspi/lib-wiring-pi`を読み込みます。
また、外部から`main`関数を参照できるように`export`しておきます。
RGBフルカラーLEDをつないだGPIOピン番号を`defconstant`で定数定義します(今回はGPIO12を使用)。
プログラム本体は`main`関数として書いていきます。流れとしては、以下の通りです。

- `init`関数

1. `wiringpi-setup-gpio`で初期化
2. `pin-mode`でGPIO12をPWM出力モードに設定
3. `pwm-set-range`でPWMジェネレータの範囲レジスタを`1024`に設定
4. `pwm-set-clock`でPWMクロックの約数を`375`に設定

- `main`関数の無限ループ

1. サーボモーターの動作角度を計算
2. `pwm-write`でPWMレジスタに標準入力した数値を設定し、サーボモーターを動かす

### サーボモーターの角度計算について

今回使用した`SG-90`のパルス幅と角度の関係は以下の通りです。

| パルス幅(ミリ秒) | 角度 |
|:---:|:---:|
| 0.5 | -90 |
| 2.4 |  90 |

0.5〜2.4ミリ秒の間でパルス幅を調整すれば、目的の角度まで動かすことができます。
ハードウェアPWMの制御では1周期20ミリ秒を1024段階に分割した値でパルス幅を指定します。

- 0.5ミリ秒のパルス幅なら `1024 / 20 * 0.5 = 25.6 ≒ 26`
- 2.4ミリ秒のパルス幅なら `1024 / 20 * 2.4 = 122.880005 ≒ 122`

`26〜122`の間でPWMを出力すれば好きな角度まで動かせます。  
この出力値は、角度から計算して求められます。  
90度で値が`48`増えるため、1度あたり`48 / 90`となります。  
0度は74なので計算式は次の通りです。

```
74 + 48 / 90 * 動かしたい角度
```

例：45度まで動かす場合は`98`です。

### 実行

`cl-raspi.asd`に作成したパッケージ`cl-raspi/src/servomotor`を追加します。

```
(defsystem "cl-raspi"
    :class :package-inferred-system
    :version "0.1.0"
    :license "MIT"
    :depends-on ("cffi"
                 "cl-raspi/lib-wiring-pi"
                 "cl-raspi/src/servomotor"))
```

ハードウェアPWMはrootユーザーの権限でコマンドを実行する必要があります。  
なので、Lemを起動する時は`sudo lem`としてください。

`cl-raspi`を`quicklisp`でロードし`cl-raspi/src/servomotor`パッケージの`main`関数を実行します。

```
(ql:quickload :cl-raspi)
(cl-raspi/src/servomotor:main)
```

## I2C 温度センサー

I2Cとは、Inter Integrated Circuit の略で、「I2C」と書いて アイ・スクウェア・シー と呼びます。
フィリップス社で開発されたシリアルバスで、1980年代初期に提唱されました。
シリアルデータ (SDA) とシリアルクロック (SCL) の２本の信号線で情報伝達を行います。

### 使用するWiringPi関数

I2Cで必要になるWiringPiの機能を`lib-wiring-pi.lisp`に追加していきます。

- wiringPiI2CSetup  
これにより、指定されたデバイスIDでI2Cシステムが初期化されます。  
IDはデバイスのI2C番号で、これを見つけるためにi2cdetectコマンドを使用できます。  
戻り値は標準のLinuxファイルハンドルで、エラーがあれば-1を返します。

```
(defcfun ("wiringPiI2CSetup" wiringpi-i2c-setup) :int
  (fd :int))
```

- wiringPiI2CWriteReg8  
8ビットのデータ値を指示されたデバイスレジスタに書き込みます。

```
(defcfun ("wiringPiI2CWriteReg8" wiringpi-i2c-write-reg8) :int
  (fd :int) (reg :int) (data :int))
```

- wiringPiI2CReadReg16  
指示されたデバイス・レジスタから16ビットの値を読み出します。

```
(defcfun ("wiringPiI2CReadReg16" wiringpi-i2c-read-reg16) :int
  (fd :int) (reg :int))
```

コードを追加したら、他のパッケージから参照出来るように、`export`に以下を追加して下さい。

```
:wiringpi-i2c-setup
:wiringpi-i2c-write-reg8
:wiringpi-i2c-read-reg16
```

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

`cl-raspi.asd`に作成したパッケージ`cl-raspi/src/i2c-temperature-sensor`を追加します。

```
(defsystem "cl-raspi"
    :class :package-inferred-system
    :version "0.1.0"
    :license "MIT"
    :depends-on ("cffi"
                 "cl-raspi/lib-wiring-pi"
                 "cl-raspi/src/i2c-temperature-sensor"))
```

`cl-raspi`を`quicklisp`でロードし`cl-raspi/src/i2c-temperature-sensor`パッケージの`main`関数を実行します。

```
(ql:quickload :cl-raspi)
(cl-raspi/src/i2c-temperature-sensor:main)
```

## SPI 3軸加速度センサー

SPI(Serial Peripheral Interface)通信とは、同期式シリアル通信の一つです。  
SCK、SDO、SDIの3本の信号線を用いて通信を行います。

### 使用するWiringPi関数

SPIで必要になるWiringPiの機能を`lib-wiring-pi.lisp`に追加していきます。

- wiringPiSPISetup  
チャンネルを初期化する関数。（RaspberryPiには2つのチャンネル、0と1があります。）  
速度パラメータは500,000〜32,000,000の範囲の整数で、SPIクロック速度をHzで表します。

```
(defcfun ("wiringPiSPISetup" wiringpi-spi-setup) :int
  (channel :int) (speed :int))
```

- wiringPiSPIDataRW  
選択されたSPIバス上で、同時に書込み/読出しトランザクションが実行されます。  
バッファ内のデータは、SPIバスから返されたデータによって上書きされます。

```
(defcfun ("wiringPiSPIDataRW" wiringpi-spi-data-rw) :int
  (channel :int) (data :pointer) (len :int))
```

コードを追加したら、他のパッケージから参照出来るように、exportに以下を追加して下さい。

```
:wiringpi-spi-setup
:wiringpi-spi-data-rw
```

### 使用した電子部品と回路図

電子部品は次のものを使用しました。

- ３軸加速度センサモジュール LIS3DH  
販売サイト：http://akizukidenshi.com/catalog/g/gK-06791/

![LIS3DH](images/09-pic-lis3dh.png)

上記電子部品を以下のようにブレッドボードに配置します。

![回路図](images/09-circuit-diagram-lis3dh.jpg)

### プログラム本体作成

プログラム本体をsrcディレクトリ内に3-axis-acceleration-sensor.lispという名前で作成します。

```
(defpackage :cl-raspi/src/3-axis-acceleration-sensor
  (:use :cl
        :cl-raspi/lib-wiring-pi)
  (:import-from :cffi)
  (:export :main))
(in-package :cl-raspi/src/3-axis-acceleration-sensor)

(defconstant +spi-cs+ 0)                ; 対象のSPIデバイスを選択
(defconstant +spi-speed+ 100000)        ; SPIの通信速度

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

;; SPIデータの読み書き
(defun spi-data-rw (channel data &optional (len (length data)))
  (let ((mp (cffi:foreign-alloc :unsigned-char :count len :initial-contents data)))
    (digital-write +pin+ +low+)
    (wiringpi-spi-data-rw channel mp len)
    (digital-write +pin+ +high+)
    (let ((rval (loop for i from 0 below len
                   collect (cffi:mem-aref mp :unsigned-char i))))
      (cffi:foreign-free mp)
      rval)))

;; 加速度センサーで計測したデータを取得する処理
(defun spi-read (read-addr)
  (let (outdat out)
    (setf outdat (list (logior read-addr +read+) #X00))
    (setf out (spi-data-rw +spi-cs+ outdat))
    (nth 1 out)))

;; CTRL_REGに値を設定する処理
(defun spi-write (write-addr data)
  (spi-data-rw +spi-cs+ (list (logand write-addr +write+) data)))

;; 取得したデータから加速度を算出する処理
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
    
    ;; 最初はCSをHighにしておく
    (digital-write +pin+ +high+)

    ;; LIS3DHのレジスタWHO_AM_Iを読み、0x33なら正しく通信できている
    (if (equal (spi-read +who-am-i+) #X33)
        (format t "I AM LIS3DH~%") ; 正しく通信できている場合は、"I AM LIS3DH"と表示
        (return-from main nil))    ; 正しく通信できていない場合は、処理終了

    ;; CTRL_REG1に0x77をを書込み、HR/Normal/Low-power mode (400 Hz)とします。
    (spi-write +ctrl-reg1+ #X77)

    (loop
       ;; X軸のデータを取得
       (setf lb (spi-read +out-x-l+))
       (setf hb (spi-read +out-x-h+))
       (setf x  (conv-two-byte hb lb))

       ;; Y軸のデータを取得
       (setf lb (spi-read +out-y-l+))
       (setf hb (spi-read +out-y-h+))
       (setf y  (conv-two-byte hb lb))

       ;; Z軸のデータを取得
       (setf lb (spi-read +out-z-l+))
       (setf hb (spi-read +out-z-h+))
       (setf z  (conv-two-byte hb lb))

       (format t "x=~6d y=~6d z=~6d~%" x y z)

       (delay 500))))
```

最初に`cl-raspi/src/3-axis-acceleration-sensor`という名前でパッケージを定義し、`cl-raspi/lib-wiring-pi`を読み込みます。
また、外部から`main`関数を参照できるように`export`しておきます。
プログラム本体は`main`関数として書いていきます。
流れとしては、以下の通りです。

- `main`関数

1. `wiringpi-spi-setup`でSPIシステムの初期化
2. `wiringpi-setup-gpio`でGPIOの初期化
3. `pin-mode`でCS(GPIO8)を出力に設定
4. `digital-write`で最初はCSをHighにしておく
5. `spi-write`関数でCTRL\_REGに値を設定

- `main`関数 無限ループ内

1. `spi-read`関数でX/Y/Z軸方向の加速度を取得
2. `conv-two-byte`関数で加速度を算出
3. X/Y/Z軸方向の加速度をコンソール出力

- `spi-data-rw`関数

1. `foreign-alloc`で要素数lenのunsigned char型配列mpを確保して、dataを代入
2. `digital-write`でCSをLowにセット
3. `wiringpi-spi-data-rw`でSPI Read/Write 実行
4. `digital-write`でCSをHighにセット
5. 0～(len-1)までiをインクリメントしながらループ
6. `mem-aref`でmpから値を取得し、rvalへ格納
7. `foreign-free`でmpを開放

- `spi-read`関数 加速度センサーで計測したデータを取得する処理

1. 書込みデータ作成
2. SPIデータの読書き処理実行
3. リストの2つ目の値がデバイスからのデータなのでそれを返す

- `spi-write`関数  
CTRL\_REGに値を設定する処理  
CTRL\_REG1に0x77をを書込み、HR/Normal/Low-power mode (400 Hz)とする

- `conv-two-byte`関数  
取得したデータから加速度を算出する処理

### 実行

`cl-raspi.asd`に作成したパッケージ`cl-raspi/src/3-axis-acceleration-sensor`を追加します。

```
(defsystem "cl-raspi"
    :class :package-inferred-system
    :version "0.1.0"
    :license "MIT"
    :depends-on ("cffi"
                 "cl-raspi/lib-wiring-pi"
                 "cl-raspi/src/3-axis-acceleration-sensor"))
```

cl-raspiをquicklispでロードし`cl-raspi/src/3-axis-acceleration-sensor`パッケージのmain関数を実行します。

```
(ql:quickload :cl-raspi)
(cl-raspi/src/3-axis-acceleration-sensor)
```

## 終わりに

ここまで読んでくださってありがとうございます。
Raspberry Pi での電子工作では Python が主流となっているようですが、Common Lisp でもできるよという話でした。
ラッパーを用意しなければいけないため少々面倒ではありますが、必要なものをその都度調べて使うため機能を理解できるし、一度作ってしまえば後は使いまわせるので気にするほど手間ではないと思っています。
簡単なことしか書けてないですが、基本的なことは一通り書けたと思います。
これを読んでCommon LispでRaspberry Pi電子工作をエンジョイして頂ければ幸いです。
