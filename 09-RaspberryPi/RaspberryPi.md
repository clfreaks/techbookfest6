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
git clone git@github.com:roswell/roswell.git
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

執筆時での最新バージョンは`19.1.10.96(b4b4gac)`でした。

## Roswellの設定

```
ros setup
```

## Common Lispをインストール

SBCL(Steel Bank Common Lisp)をインストール

```
ros install sbcl-bin
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

## タクトスイッチでGPIO入力

## シリアル通信

## I2C 温度センサー

## SPI 3軸加速度センサー

## I2C LCD

## OLED
