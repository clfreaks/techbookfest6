
= エディタ「Lem」


LemはCommon Lispが動作するランタイム上のエディタです。



Common Lispでの拡張を想定しており、Common Lispの開発環境を主に提供しています。  それ以外にもLemの開発者が使う言語を中心にサポートしています。
操作体系は作者がemacsを使っていたこともあり、emacsによく似ていますが他のエディタにある機能も取り込むようにしており、vi(vim)のモードが用意されています。



また、メジャーなCommon Lisp処理系にあるイメージダンプ機能の利点を活かして、ライブラリの読み込み、メモリへのデータの配置状態を実行ファイルの形にしていて、高速に起動できる特徴もあります。


== Lemのインストール


LemをインストールするにはRoswellを使うのが簡単です。
次のコマンドでインストールできます。


//emlist{
$ ros install cxxxr/lem
//}


lemは256色をサポートしたターミナルで使うことを想定しています。
linuxのターミナルの場合、デフォルトでは8色しか表示できない事が多いのでTERM変数を変更してください。


//emlist{
$ export TERM=xterm-256color
//}

== Lemで使う用語


LemではEmacsと同じようにControlやMetaをプリフィクスとするコマンドを使います。



@<b>{C-} コントロールキーを押しっぱなしにして別のキーを打つことを意味します。



@<b>{M-} メタキーを押しっぱなしにして別のキーを打つことを意味します。メタキーはAltキーを使い、Macのターミナルでは設定でOptionキーに割り当てられます。



たとえば
@<tt>{C-x o}はControlを押しながらxを押したあと、Controlを離してoを押します。
@<tt>{C-x C-o}だとControlを押しながらxを押し、更にControlを押しながらoを押します。

== Lemの起動と終了


コマンドラインからLemを起動するにはlemコマンドを使います。
lemコマンドはRoswellからインストールしている場合に使えます。


//emlist{
$ lem [ファイル名]
//}


終了するには@<tt>{C-x C-c}と入力してください。


== 基本的な使い方

項立て案。
* slime の起動 (start-lisp-repl いらなさそう?) (c-u m-x slime)
* repl (評価、履歴、listener-clear-buffer、lisp-repl-interrupt)
* find-file -> c-x c-s
* lisp-mode (でc-c c-e)
* カーソルの移動(ここは努力目標)
* bufferの切り替え、削除
* windowの分割と削除
