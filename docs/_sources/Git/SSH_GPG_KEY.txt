=====================
SSH Key / GPG Key
=====================

| GitにおけるSSH鍵とGPG鍵について記述する.

SSH Key
========

| 暗号化方式はECDSAを採用する.
| 今後RSAは廃止され, ECDSAがデフォルトになるだろうからである.

1. opensslのバージョンを上げる (必要なければいい)

.. block::

    $ brew upgrade openssl
    $ brew link openssl --force

2. 鍵を作成, 適宜名前修正, 公開鍵の中身をGitHubのSSH keysに登録.

.. block::

    $ ssh-keygen -t ecdsa -b 256 -C "your_email@example.com"
    $ cat .ssh/id_ecdsa.pub | pbcopy

3. 疎通確認

.. block::

    $ ssh -T -i ~/.ssh/id_ecdsa git@github.com

4. 必要に応じてconfigを修正

GPG Key
=========

GPGとは
---------

| GNU Privacy Guard (GnuPG, GPG)とは, GPLに基づいた暗号化ソフトのこと.
| GPGで作成した鍵を利用し暗号化・復号化できる.
| gitやGitHubではGPGを利用した署名に対応しており, コミットやマージ, タグ生成の際に署名ができる. これによりその内容の信頼性を上げる.
| GitHubでは署名付きコミットにバッジが表示されるようになる.

GPG install
--------------

| 以下でGUIアプリケーションとGPGバイナリがインストールされる

.. block::

    $ brew cask install gpg-suite


GPGで暗号化鍵を生成
--------------------

ECDSAを指定する(future-default)
鍵に有効期限を設定する (2年後の3月1日)

.. block::

    $ gpg --quick-generate-key "aztecher <mikiyaf.business@gmail.com>" future-default - 2021-03-01

作成した鍵は以下のコマンドで確認できる.

.. block::

    $ gpg --list-secret-keys --keyid-format LONG

生成時の出力結果, もしくは上記コマンドでIDを確認できる.

GitHubにGPG公開鍵を登録する
----------------------------

SSH鍵を登録するときのようにクリップボードにコピーして登録

.. block::

    $ gpg --armor --export [ID] | pbcopy

署名付きでコミットをできるようにする
--------------------------------------

グローバル設定を少しいじってやる.
これは個人の裁量次第. Gitのユーザーを分けたいとか色々あるからね.

.. block::

    $ git config --global user.signingkey [ID]

ちゃんと設定できているかどうかは以下のコマンドで確認できる.

.. block::

    $ git config --get user.signingkey


署名しているはずがVerifyされていない
--------------------------------------

これは直面した問題. 結果的には `.gitconfig` 内に記述していたemailアドレスをtypoしていたためである. 逆に言うとココらへんの情報の齟齬があると正しく認識してくれないようなので気をつける.
