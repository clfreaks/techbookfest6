<title>Raspberry PiでCommon Lispを使おう</title>

# はじめに



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

# 電子工作してみよう

## Lチカ

## タクトスイッチでGPIO入力

## シリアル通信

## I2C 温度センサー

## SPI 3軸加速度センサー

## I2C LCD

## OLED
