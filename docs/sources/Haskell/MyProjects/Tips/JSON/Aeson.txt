==================
Aeson
==================

ここでは, HaskellでJSONを取り扱うためのパッケージである `aeson` に関して記述していく.

Introduction
==============

| 基本的にJSONデータと, 定義したデータ型を相互に変換する方法には

* 自前で変換関数を定義する
* Generic型クラスを用いた自動導出
* Template Haskellを用いた自動導出

| がある.
| 面倒だが最も柔軟なのが一番上.
| 自動導出の場合は, JSONデータと定義したデータ型が単純に変換できる場合に自動的に変換関数を定義してくれる.
| 通常は自動導出を行うほうが便利だろう.


Aeson
========

Basic
=======

with Generic Type Class
========================

Generic型クラスを用いると, 変換関数の定義を省略して自動導出できる.

.. code-block:: hs

    {-# LANGUAGE DeriveGeneric #-}

    import Data.Aeson
    import GHC.Generics

    data Foo = Foo { id :: Int, content :: String } deriving (Show, Generic)

    instance FromJSON Foo
    instance ToJSON Foo

Generic型クラスについての詳細は 「Genericへのリンク」へ記載する.
やらないといけないことは,

* `DerivingGeneric` 言語拡張
* `GHC.Generics` パッケージのインポート
* データ型のderiving宣言に, `Generic` を追加

この3点だけ

with Template Haskell
======================


