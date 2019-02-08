# Raspberry PiでCommon Lispを使おう

# はじめに

Raspberry Piで電子工作と言えば、Pythonで紹介している本や記事がとても多いです。
しかし、自分はLisperなのでCommon Lispを使ってこれをやっていきます。

# 環境の構築

ハードは`Raspberry Pi 3`、OSは`Raspbian Stretch`を使用します。
OSはインストールされいるものとします。

## Roswellのインストール

Roswellをインストールするためにまずは、Linuxbrewのインストールします。

```
sudo apt install linuxbrew-wrapper
```

Linuxbrewをインストールしたら以下のコマンドでRoswellをインストールします。

```
brew install roswell
```

## Roswellの設定

```
ros setup
```

## Common Lispをインストール

SBCL(Steel Bank Common Lisp)またはCCL(Clozure Common Lisp)をお好みでインストール

```
ros install sbcl
```

```
ros install ccl-bin
```

## 動作確認

REPLを起動してCommon Lispがちゃんと使えるか確認します。

```
ros run
```

## GPIO制御ライブラリについて

GPIO制御ライブラリとして`Wiring Pi`を使用します。

公式サイト：[http://wiringpi.com/](http://wiringpi.com/)

ラッパーを作成しCommon Lispから呼び出して使用します。

# 電子工作してみよう

## Lチカ

まず最初にLチカに必要となる各種関数をCommon Lispで使えるようにします。
Lチカで必要になる関数は以下の4つです。

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

ラッパーを`libwiringPi.lisp`として以下のように作成します。

```
(define-foreign-library libwiringPi
              (:unix "libwiringPi.so"))

(use-foreign-library libwiringPi)

;; wiringPi初期化
(defcfun "wiringPiSetupGpio" :int)

;; GPIOピンのモード設定
(defcfun "pinMode" :void (pin :int) (mode :int))

;; GPIOピンの出力制御
(defcfun "digitalWrite" :void (pin :int) (value :int))

;; 待機処理
(defcfun "delay" :void (howlong :uint))
```

実際にLチカするプログラムは以下のようになります。

```
;; GPIO11(23ピン)を「+pin+」の名前で定義
(defconstant +pin+ 11)

(defun blink ()
  ;; GPIOを初期化
  (wiringPiSetupGpio)

  ;; GPIO11を出力モード(1)に設定
  (pinMode +pin+ 1)

  ;; 無限ループ(Ctrl-cでループを抜ける
  (loop
    ;; GPIOを3.3VにしてLEDを点灯
    (digitalWrite +pin+ 1)
    ;; 500ms待機
    (delay 500)
    ;; GPIOを0VにしてLEDを消灯
    (digitalWrite +pin+ 0)
    ;; 500ms待機
    (delay 500)))
```

## タクトスイッチでGPIO入力

## シリアル通信

## I2C 温度センサー

## SPI 3軸加速度センサー

## I2C LCD

## OLED
