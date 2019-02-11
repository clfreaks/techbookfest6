# Raspberry Pi��Common Lisp��Ȥ���

# �Ϥ����

Raspberry Pi���Żҹ���ȸ����С�Python�ǾҲ𤷤Ƥ����ܤ䵭�����ȤƤ�¿���Ǥ���  
����������ʬ��Lisper�ʤΤ�Common Lisp��ȤäƤ�����äƤ����ޤ���  

# �Ķ�����

�ϡ��ɤ�`Raspberry Pi 3`��OS��`Raspbian Stretch`����Ѥ��ޤ���  
`Raspbian Stretch`�ϰʲ��Υߥ顼�����Ȥ�Ȥ��ȸ��������Ȥ����᤯��������ɽ���ޤ���  
����ϼ�ɮ���Ǥκǿ���`raspbian-2018-11-15`����Ѥ��Ƥ��ޤ���  

[http://ftp.jaist.ac.jp/pub/raspberrypi/raspbian/images/](http://ftp.jaist.ac.jp/pub/raspberrypi/raspbian/images/)

## Roswell�Υ��󥹥ȡ���


�ޤ��ϡ�Roswell�򥤥󥹥ȡ��뤹�뤿���ɬ�פʤ�Τ򥤥󥹥ȡ��뤷�ޤ���  
���󥹥ȡ��뤹��Τϰʲ���3�ĤǤ���  

- autoconf
- automake
- libcurl4-openssl-dev

```
sudo apt install autoconf automake libcurl4-openssl-dev
```

GitHub����Roswell�Υ����������ɤ��������ɤ��ޤ���

```
git clone -b release https://github.com/roswell/roswell.git
```

��������ɤ���Roswell�Υǥ��쥯�ȥ�ذ�ư���ޤ���

```
cd roswell
```

�ʲ��Υ��ޥ�ɤ��˼¹Ԥ������󥹥ȡ����Ԥ��ޤ���

```
./bootstrap
./configure
make
sudo make install
```

Roswell�ΥС��������ǧ

```
ros --version
```

��ɮ���Ǥκǿ��С�������`19.1.10.96`�Ǥ�����

���󥹥ȡ��뤬����ä���ʲ��Υ��ޥ�ɤǽ�������Ԥ��ޤ���

```
ros setup
```

## Common Lisp�򥤥󥹥ȡ���

�����Common Lisp�����Ϥ�1�ĤǤ���`CCL (Clozure Common Lisp)`��Ȥ��ޤ���

```
ros install ccl-bin
```

�ʲ��Υ��ޥ�ɤǥС��������ǧ���ޤ���

```
ros run -- --version
```

��ɮ���ΥС�������`Version 1.11/v1.11.5 (LinuxARM32)`�Ǥ�����

## Lem�Υ��󥹥ȡ���

Common Lisp�Ǻ������줿���ǥ����Ǥ���`Lem`�򥤥󥹥ȡ��뤷�ޤ���  
`ncurses`��ɬ�פʤΤǡ��ǽ�˥��󥹥ȡ��뤷�Ƥ����ޤ���

```
sudo apt install libncurses5-dev libncursesw5-dev
```

Roswell��Lem�򥤥󥹥ȡ��뤷�ޤ���

```
ros install cxxxr/lem
```

�Ķ��ѿ�����Ͽ����bash�������ɤ߹��ߤ��ޤ���

```
echo export PATH=$PATH:~/.roswell/bin >> ~/.bash_profile
source ~/.bash_profile
```

�Ĥ��Ǥˡ�`sudo`���Ǥ�Lem��Ȥ���褦�����ꤷ�ޤ���  
�ʲ��Υ��ޥ�ɤ�`sudoers.tmp`�ե�����򳫤��Խ����ޤ���

```
sudo visudo
```

`env_reset`��`secure_path`������򥳥��ȥ����Ȥ���PATH���ݻ�����������ɵ����ޤ���

```
Defaults env_keep += "PATH"
```

�ʲ��Τ褦�ˤʤäƤ�����ɤ��Ǥ���  

![visudo](https://github.com/clfreaks/techbookfest6/blob/master/09-RaspberryPi/pic/visudo.png)

�ʲ��Υ��ޥ�ɤǥС��������ǧ���ޤ���

```
lem --version
```

��ɮ���ΥС�������`lem 1.5 (ARM-raspberrypi)`�Ǥ�����  

��ư������ϡ��ʲ��Υ��ޥ�ɤ�¹Ԥ��ޤ���

```
lem --frontend ncurses-ccl
```

��󤳤���ǤĤΤ����ݤʤΤǡ������ꥢ�������ꤷ`lem`�����ǵ�ư�Ǥ���褦�ˤ��ޤ���  
bash����κ��ɤ߹��ߤ�˺�줺�ˡ�

```
echo alias lem=\'lem --frontend ncurses-ccl\'  >> ~/.bash_profile
source ~/.bash_profile
```

�Ĥ��Ǥˡ�sudo���Ǥ⥨���ꥢ����Ȥ���褦�ˤ��뤿��˰ʲ���������ɲä��ޤ���

```
echo alias sudo=\'sudo \' >> ~/.bash_profile
source ~/.bash_profile
```

��ư�����Ȥ��ν�����̤ϰʲ��Τ褦�ˤʤ�ޤ���  

![lem-pic-001](https://github.com/clfreaks/techbookfest6/blob/master/09-RaspberryPi/pic/lem-pic-001.png)

���ξ��֤ǡ�`M-x start-lisp-repl`���ޥ�ɤ�¹Ԥ����`REPL(Read-Eval-Print Loop)`����ư���ޤ���  

![lem-pic-002](https://github.com/clfreaks/techbookfest6/blob/master/09-RaspberryPi/pic/lem-pic-002.png)

����Ū�ˤ����ǥץ�����¹Ԥ��Ƥ����ޤ���  

![lem-pic-003](https://github.com/clfreaks/techbookfest6/blob/master/09-RaspberryPi/pic/lem-pic-003.png)

## GPIO����饤�֥��ˤĤ���

GPIO����饤�֥��Ȥ���`Wiring Pi`����Ѥ��ޤ���  
`Raspbian Stretch`�ˤϺǽ餫�饤�󥹥ȡ��뤵��Ƥ��ޤ���  

���������ȡ�[http://wiringpi.com/](http://wiringpi.com/)  

��åѡ��������Common Lisp����ƤӽФ��ƻ��Ѥ��ޤ���

# �Żҹ���Ƥߤ褦

�ޤ��ϡ��ץ��������ѥǥ��쥯�ȥ��������ޤ���

```
cd ~/.roswell/local-projects
mkdir my-dir (��)
cd my-dir
mkdir cl-raspi
```

(��) Github��������Ȥ���äƤ���ΤǤ���С��桼����̾��Ȥ����ɤ��Ǥ���  

�ץ��������ѥǥ��쥯�ȥ깽���ϰʲ��Τ褦�ˤ��ޤ���

```
cl-raspi
  ���� cl-raspi.asd
  ���� lib-wiring-pi.lisp
  ���� src
       ���� ...
```

`cl-raspi.asd`����Ȥϰʲ��Τ褦�ˤ��ޤ���

```common-lisp
(defsystem "cl-raspi"
    :class :package-inferred-system
    :version "0.1.0"
    :license "MIT"
    :depends-on (;; ���Ѥ���饤�֥��(�����CFFI)
                 "cffi"
                 ;; ��åѡ�
                 "cl-raspi/lib-wiring-pi"
                 ;; �ץ��������������ʲ����ɲä��Ƥ���
                 "cl-raspi/src/..."))
```

`lib-wiring-pi.lisp`�ϥ�åѡ��Ǥ���  
������WiringPi�δؿ����ɲä��Ƥ����ޤ���

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

�Ǹ�ˡ�REPL�ǰʲ��Υ��ޥ�ɤ�¹Ԥ���ȥץ������Ȥ���Ͽ����ޤ���  

```
(ql:register-local-projects)
```

## L����

L������ɬ�פˤʤ�WiringPi�δؿ��ϰʲ���4�ĤǤ���

- wiringPiSetupGpio  
wiringPi�ν�����˻��ѡ�  
���顼�ξ���-1���֤äƤ��ޤ���  

- pinMode  
GPIO�ԥ�Υ⡼�������Ԥ��ޤ���  
��1������GPIO�ԥ��ֹ桢��2�����˥⡼��(0��Input��1��Output)�����ꡣ  

- digitalWrite  
GPIO�ԥ�ν��������Ԥ��ޤ���  
��1������GPIO�ԥ��ֹ桢��2��������(0 or 1)�����ꡣ  

- delay  
�Ե�������Ԥ��ޤ���  
�����ǻ��ꤷ����[�ߥ���)ʬ�Ե����ޤ���  

`lib-wiring-pi.lisp`�˰ʲ��Υץ������ɵ����ޤ���

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

�ץ�������Τ�`src`�ǥ��쥯�ȥ����`blink.lisp`�Ȥ���̾���Ǻ������ޤ���

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

`cl-raspi`��`quicklisp`�ǥ��ɤ�`cl-raspi/src/blink`�ѥå�������`main`�ؿ���¹Ԥ��ޤ���

```common-lisp
(ql:quickload :cl-raspi)
(cl-raspi/src/blink:main)
```

## �����ȥ����å���GPIO����

## ���ꥢ���̿�

## I2C ���٥��󥵡�

## SPI 3����®�٥��󥵡�

## I2C LCD

## OLED
