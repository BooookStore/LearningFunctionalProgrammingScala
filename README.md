Learning Functional Programming Scala
==========

## Overview
関数型言語をScalaを通して学習するために、私が記述したコードです。学習に際しては以下の書籍を参考にしました。
 - [Functional Programming in Scala](https://www.amazon.co.jp/Functional-Programming-Scala-Paul-Chiusano/dp/1617290653)

## Directory

- **datastructure** <br>
関数型プログラミングにおけるデータ構造の定義と、データ構造に対する操作を定義したコンパニオンオブジェクトの定義

- **errorhandling** <br>
関数型プログラミングにおける例外処理に関するコード

- **language** <br>
プログラミング言語Scalaに関するコード

- **nonStrictFunction** <br>
関数型プログラミングにおける遅延評価に関するコード

- **purelyfunctionalstate** <br>
状態を操作する純粋関数に関するコード

## How to build

Simple Build Tool を利用してソースコードをコンパイルできます。コンパイル方法は以下のコマンドを入力します。
```shell:
sbt compile
```

Simple Build Tool については[こちら](http://www.scala-sbt.org/0.13/docs/index.html)を参考しました。
