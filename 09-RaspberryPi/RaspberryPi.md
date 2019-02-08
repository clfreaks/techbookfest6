# Raspberry Pi��Common Lisp��Ȥ���

# �Ϥ����

Raspberry Pi���Żҹ���ȸ����С�Python�ǾҲ𤷤Ƥ����ܤ䵭�����ȤƤ�¿���Ǥ���
����������ʬ��Lisper�ʤΤ�Common Lisp��ȤäƤ�����äƤ����ޤ���

# �Ķ��ι���

�ϡ��ɤ�`Raspberry Pi 3`��OS��`Raspbian Stretch`����Ѥ��ޤ���
OS�ϥ��󥹥ȡ��뤵�줤���ΤȤ��ޤ���

## Roswell�Υ��󥹥ȡ���

Roswell�򥤥󥹥ȡ��뤹�뤿��ˤޤ��ϡ�Linuxbrew�Υ��󥹥ȡ��뤷�ޤ���

```
sudo apt install linuxbrew-wrapper
```

Linuxbrew�򥤥󥹥ȡ��뤷����ʲ��Υ��ޥ�ɤ�Roswell�򥤥󥹥ȡ��뤷�ޤ���

```
brew install roswell
```

## Roswell������

```
ros setup
```

## Common Lisp�򥤥󥹥ȡ���

SBCL(Steel Bank Common Lisp)�ޤ���CCL(Clozure Common Lisp)�򤪹��ߤǥ��󥹥ȡ���

```
ros install sbcl
```

```
ros install ccl-bin
```

## ư���ǧ

REPL��ư����Common Lisp�������ȻȤ��뤫��ǧ���ޤ���

```
ros run
```

## GPIO����饤�֥��ˤĤ���

GPIO����饤�֥��Ȥ���`Wiring Pi`����Ѥ��ޤ���

���������ȡ�[http://wiringpi.com/](http://wiringpi.com/)

��åѡ��������Common Lisp����ƤӽФ��ƻ��Ѥ��ޤ���

# �Żҹ���Ƥߤ褦

## L����

�ޤ��ǽ��L������ɬ�פȤʤ�Ƽ�ؿ���Common Lisp�ǻȤ���褦�ˤ��ޤ���
L������ɬ�פˤʤ�ؿ��ϰʲ���4�ĤǤ���

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

��åѡ���`libwiringPi.lisp`�Ȥ��ưʲ��Τ褦�˺������ޤ���

```
(define-foreign-library libwiringPi
              (:unix "libwiringPi.so"))

(use-foreign-library libwiringPi)

;; wiringPi�����
(defcfun "wiringPiSetupGpio" :int)

;; GPIO�ԥ�Υ⡼������
(defcfun "pinMode" :void (pin :int) (mode :int))

;; GPIO�ԥ�ν�������
(defcfun "digitalWrite" :void (pin :int) (value :int))

;; �Ե�����
(defcfun "delay" :void (howlong :uint))
```

�ºݤ�L��������ץ����ϰʲ��Τ褦�ˤʤ�ޤ���

```
;; GPIO11(23�ԥ�)���+pin+�פ�̾�������
(defconstant +pin+ 11)

(defun blink ()
  ;; GPIO������
  (wiringPiSetupGpio)

  ;; GPIO11����ϥ⡼��(1)������
  (pinMode +pin+ 1)

  ;; ̵�¥롼��(Ctrl-c�ǥ롼�פ�ȴ����
  (loop
    ;; GPIO��3.3V�ˤ���LED������
    (digitalWrite +pin+ 1)
    ;; 500ms�Ե�
    (delay 500)
    ;; GPIO��0V�ˤ���LED�����
    (digitalWrite +pin+ 0)
    ;; 500ms�Ե�
    (delay 500)))
```

## �����ȥ����å���GPIO����

## ���ꥢ���̿�

## I2C ���٥��󥵡�

## SPI 3����®�٥��󥵡�

## I2C LCD

## OLED
