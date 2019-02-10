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

Common Lispで作成されたエディタである`Lem`をインストールします。__
`ncurses`が必要なので、最初にインストールしておきます。


```
sudo apt install libncurses5-dev libncursesw5-dev
```


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

## GPIO制御ライブラリについて

GPIO制御ライブラリとして`Wiring Pi`を使用します。  
`Raspbian Stretch`には最初からインストールされています。  

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
