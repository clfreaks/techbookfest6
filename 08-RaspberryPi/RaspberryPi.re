
= Raspberry Pi電子工作

== はじめに


@<tt>{Raspberry Pi}で電子工作と言えば、@<tt>{Python}を使って紹介している記事や本が多いですが、@<tt>{Common Lisp}でも同じようにできます。
いくつかの基本的な電子工作の例を挙げ、最後に実践として温度計付きデジタル時計の作り方について説明したいと思います。


== 今回使用したRaspberry PiとOS


ハードは@<tt>{Raspberry Pi 3}、OSは@<tt>{Raspbian Stretch}を使用します。
@<tt>{Raspbian Stretch}はミラーサイト@<fn>{raspbian-mirror-site}を使うと公式サイトよりも早くダウンロード出来ます。
執筆時での最新版は@<tt>{raspbian-2018-11-15}です。

//footnote[raspbian-mirror-site][http://ftp.jaist.ac.jp/pub/raspberrypi/raspbian/images/]


== Common Lisp環境構築


=== Roswellインストール

Roswellは基本的に@<tt>{homebrew (Linuxではlinuxbrew)}でインストールしますが、@<tt>{homebrew}がRaspberry PiのCPUであるARM32をサポートしていないため、ソースコードをビルドしてインストールします。(Roswellのwiki@<fn>{roswell-wiki}を参照)

//footnote[roswell-wiki][https://github.com/roswell/roswell/wiki/Installation#building-from-source]


=== Common Lispインストール

ARM32の@<tt>{SBCL}がスレッド対応していないため、今回は@<tt>{CCL}を使用します。

//emlist{
$ ros install ccl-bin
//}


== GPIO制御ライブラリについて


=== WiringPi

GPIO制御ライブラリとして@<tt>{Wiring Pi}@<fn>{wiring-pi-official-site}を使用しています。
これは、@<tt>{Raspbian Stretch}には最初からインストールされているので別途インストールする必要はありません。

=== CFFI

Common LispからはCFFI@<fn>{cffi-official-site}@<fn>{cffi-user-manual}でラッパーを作成して呼び出しています。
CFFIとは、Common LispからC言語で作成された関数を呼び出すためのインターフェースです。
今回、ラッパーは自分が用意したのでそれをインストールして下さい。@<fn>{cl-raspi-github}

//emlist{
$ ros install fireflower0/cl-raspi
//}

@<tt>{Quicklisp}でロードして使用して下さい。

//emlist{
(ql:quickload :cl-raspi)
//}


//footnote[wiring-pi-official-site][http://wiringpi.com/]
//footnote[cffi-official-site][https://common-lisp.net/project/cffi/]
//footnote[cffi-user-manual][https://common-lisp.net/project/cffi/manual/index.html]
//footnote[cl-raspi-github][https://github.com/fireflower0/cl-raspi]


== 基本的な電子工作


ここでは基本的な電子工作の例としてLEDを点滅させるLチカと、I2C温度センサーの使い方について簡単に説明します。


=== LED


最初は電子工作の基本、Lチカです。
電子部品は次のものを使用しました。

 * 赤色LED 1個
 * 330Ω抵抗(橙橙茶金) 1個

上記電子部品を以下のようにブレッドボードに配置します。

//image[09-circuit-diagram-blink][LEDと抵抗の配線図][scale=0.5]{
//}

プログラムは、@<tt>{cl-raspi/src}ディレクトリ下に@<tt>{blink.lisp}という名前で作成してあります。

//emlist{
(defpackage :cl-raspi/src/blink
  (:use :cl
        :cl-raspi/lib-wiring-pi) ; ラッパーをインポート
  (:export :main))               ; main関数をエクスポート
(in-package :cl-raspi/src/blink)

(defconstant +pin+ 11)           ; GPIOピン番号(今回はGPIO11)を定数定義

(defun main ()
  (wiringpi-setup-gpio)          ; 1. GPIOを初期化
  (pin-mode +pin+ +output+)      ; 2. GPIO11を出力モードに設定
  (loop
     (digital-write +pin+ 1)     ; 3. GPIO11の電圧のHigh(1)に設定
     (delay 500)                 ; 4. 500ms待機
     (digital-write +pin+ 0)     ; 3. GPIO11の電圧のLow(0)に設定
     (delay 500)))               ; 4. 500ms待機
//}


@<tt>{cl-raspi/src/blink}の@<tt>{main}関数を実行して下さい。

//emlist{
(cl-raspi/src/blink:main)
//}

=== I2C 温度センサー


次は温度センサーの制御です。
電子部品は次のものを使用しました。

 * ADT7410を使用した温度センサーモジュール@<fn>{i2c-temperature-sensor}

//footnote[i2c-temperature-sensor][http://akizukidenshi.com/catalog/g/gM-06675/]

上記電子部品を以下のようにブレッドボードに配置します。

//image[09-circuit-diagram-adt7410][温度センサーの配線図][scale=0.5]{
//}

@<tt>{ADT7410}は@<tt>{I2C}と呼ばれる通信規格を採用しているので、データを送受信する@<tt>{SDA}、通信の同期を取る@<tt>{SCL}、電源@<tt>{VDD}、@<tt>{GND}の4本ケーブルを繋ぐだけで動作します。

プログラムは、@<tt>{cl-raspi/src}ディレクトリ下に@<tt>{i2c-temperature-sensor.lisp}という名前で作成してあります。

//emlist{
(defpackage :cl-raspi/src/i2c-temperature-sensor
  (:use :cl
        :cl-raspi/lib-wiring-pi) ; ラッパーをインポート
  (:export :main))               ; main関数をエクスポート
(in-package :cl-raspi/src/i2c-temperature-sensor)

(defconstant +i2c-addr+ #X48)    ; I2C温度センサーのアドレスを定数定義

;; バイトスワップ処理
(defun byte-swap (num-value)
  (let* ((str-value  (write-to-string num-value :base 16)) ; 数値を16進数の文字列に変換
         (temp-msb   (subseq str-value 0 2))               ; 上位ビットを取得
         (temp-lsb   (subseq str-value 2)))                ; 下位ビットを取得
    (parse-integer (concatenate 'string temp-lsb temp-msb) ; 上位/下位を入れ替え数値に変換
                   :radix 16)))

;; 取得したデータを温度データに変換する処理
(defun get-data (fd)
  (* (byte-swap (wiringpi-i2c-read-reg16 fd #X00)) 0.0078))

(defun main ()
  (let ((fd (wiringpi-i2c-setup +i2c-addr+))) ; I2Cシステムを初期化
    ;; レジスタ 0x03 に 0x80 を書き込むことで16ビットの高精度で温度を取得できる
    (wiringpi-i2c-write-reg8 fd #X03 #X80)
    ;; 温度データ取得しコンソールに出力
    (format t "~d~%" (get-data fd))))
//}

@<tt>{i2c-temperature-sensor}の@<tt>{main}関数を実行して下さい。

//emlist{
(cl-raspi/src/i2c-temperature-sensor:main)
//}


== 実践：温度計付きデジタル時計


実践として温度計付きデジタル時計を作ってみましょう。
電子部品は次のものを使用しました。

 * 16桁×2行英数カナ表示液晶モジュール【MI2CLCD-01】@<fn>{mi2clcd-01}@<br>{}搭載液晶：BO1602DGRNJB@<br>{}値段：1000円
 * ADT7410を使用した温度センサーモジュール@<br>{}値段：500円

//footnote[mi2clcd-01][https://www.marutsu.co.jp/pc/i/137795/]

上記電子部品を以下のようにブレッドボードに配置します。

//image[09-circuit-diagram-simple-temperature][回路図][scale=0.5]{
//}

LCDの一行目に日時、二行目に温度が表示されるようにプログラムを作成します。
LCDの制御を入れるとコードが長くなるので@<tt>{cl-raspi/bo1602dgrnjb}としてまとめました。
使い方は@<tt>{bo1602dgrnjb-init}で初期化して、@<tt>{bo1602dgrnjb-text}で何行目に文字列を表示するか指定します。

プログラムは、@<tt>{cl-raspi/src}ディレクトリ下に@<tt>{simple-temperature.lisp}という名前で作成してあります。

//emlist{
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
    (bo1602dgrnjb-init)   ; LCDの初期化
    (wiringpi-i2c-write-reg8 adt7410-fd #X03 #X80)
    (loop
      ;; 1行目に日時を表示
      (bo1602dgrnjb-text 1 (get-date))
      ;; 2行目に温度を表示
      (bo1602dgrnjb-text 2 (format nil "temp:~,2f" (get-data adt7410-fd)))
      (delay 1000))))
//}

@<tt>{simple-temperature}の@<tt>{main}関数を実行して下さい。

//emlist{
(cl-raspi/src/simple-temperature:main)
//}

実行すると以下のようになります。

//image[09-simple-temperature-pic][実行中の様子][scale=0.5]{
//}


== 終わりに


ここまで読んでくださってありがとうございます。
Raspberry Pi での電子工作では Python が主流となっているようですが、Common Lisp でもできるよという話でした。
これを読んでCommon LispでもRaspberry Pi電子工作をエンジョイして頂ければ幸いです。
