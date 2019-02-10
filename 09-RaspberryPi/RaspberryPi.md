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

Common Lisp�Ǻ������줿���ǥ����Ǥ���`Lem`�򥤥󥹥ȡ��뤷�ޤ���__
`ncurses`��ɬ�פʤΤǡ��ǽ�˥��󥹥ȡ��뤷�Ƥ����ޤ���


```
sudo apt install libncurses5-dev libncursesw5-dev
```


```
ros install cxxxr/lem
```

�Ķ��ѿ�����Ͽ����bash�������ɤ߹��ߤ��ޤ���

```
echo export PATH=$PATH:~/.roswell/bin >> ~/.bashrc
source ~/.bashrc
```

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
echo alias lem=\'lem --frontend ncurses-ccl\'  >> ~/.bashrc
source ~/.bashrc
```

## GPIO����饤�֥��ˤĤ���

GPIO����饤�֥��Ȥ���`Wiring Pi`����Ѥ��ޤ���  
`Raspbian Stretch`�ˤϺǽ餫�饤�󥹥ȡ��뤵��Ƥ��ޤ���  

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
