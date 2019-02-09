# Raspberry Pi��Common Lisp��Ȥ���

# �Ϥ����

Raspberry Pi���Żҹ���ȸ����С�Python�ǾҲ𤷤Ƥ����ܤ䵭�����ȤƤ�¿���Ǥ���  
����������ʬ��Lisper�ʤΤ�Common Lisp��ȤäƤ�����äƤ����ޤ���  

# �Ķ��ι���

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
git clone git@github.com:roswell/roswell.git
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

��ɮ���Ǥκǿ��С�������`19.1.10.96(b4b4gac)`�Ǥ�����

## Roswell������

```
ros setup
```

## Common Lisp�򥤥󥹥ȡ���

SBCL(Steel Bank Common Lisp)�򥤥󥹥ȡ���

```
ros install sbcl-bin
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

## �����ȥ����å���GPIO����

## ���ꥢ���̿�

## I2C ���٥��󥵡�

## SPI 3����®�٥��󥵡�

## I2C LCD

## OLED
