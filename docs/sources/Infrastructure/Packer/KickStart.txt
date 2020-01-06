==============
KickStart
==============

.. note::

    本来, KickStartとPackerは独立したツールであるが, KickStartに関しては現状Packerでの利用以外を考えていないため, このディレクトリに説明書きを載せておく.


kickstartはOSのインストールを自動化する仕組みである.
anaconda社が提供するインストールの仕組みで, Redhat系のOSが採用しているらしい.

Ubuntuの場合は, Preseedという別の仕組みが提供されているそうだ. :doc:`./Preseed.rst`

kickstartを利用することで, 上述した通りOSインストールの自動化ができる.
つまり, OSインストールの際に我々が設定する, 「言語設定」「ホスト名」...などの入力すべき情報を自動で設定してあげることができるものということ.


kickstartが担う領域
--------------------

kickstartにはprovisioning機能も付いているらしい. そうなるとAnsible等のプロビジョニングツールとの棲み分けの話になる. もちろん明確なすみ分けがあるわけではないが, kickstartが担う領域を「Bootstrapping」の領域, Ansibleが担う領域を「Configuration」「Orchestration」の領域という棲み分けにするのが一番キレイだろう.

Usage
------

実際にKickStartを利用する流れを説明する.
今回は, Virtualboxを利用して試すことにする.

Environment
^^^^^^^^^^^^^^

* Virtualbox
* OSのisoファイル
* kickstartファイル

isoファイルはダウンロードしておき, kickstartファイルはhttpでアクセスできるようにネットワーク上に置いておく.

以下に, 今回利用するkickstartファイルを記述しておく

.. code-block:: cfg

    # アップグレードするかインストールするか
    install

    # インストールタイプの設定
    # CD-ROM経由化かネットワーク経由かなど
    cdrom

    # 言語設定
    lang ja_JP.UTF-8

    # キーボード設定
    keyboard us101

    # ネットワーク設定
    # /etc/sysconfig/network-scripts/ifcfg-xxxx の部分
    network --onboot yes --device eth0 --bootproto dhcp --noipv6

    # rootのパスワード
    # xxxxxxの部分はopensshコマンドで作成するといい
    # $openssl passwd -l
    rootpw --iscrypted xxxxxx

    # iptablesの設定
    # プロビジョニングで設定するのでここでは無効にしておく
    firewall --disabled

    # 認証オプション
    authconfig --enableshadow --passalgo=sha512

    # SELinuxの設定
    # こちらも詳細はプロビジョニングで設定するので無効にする
    selinux --disabled

    # タイムゾーン
    timezone --utc Asia/Tokyo

    # bootloaderのインストール方法
    bootloader --location=mbr --driveorder=sda --append="nomodeset crashkernel=auto rhgb quiet"

    # 設定後にリブート
    reboot

    # パーティション設定
    clearpart --linux --derives=sda
    autopart

    # インストールパッケージ選択
    %package --nobase
    @core

    %end

これに関しては公式のドキュメントが充実しているため, そちらを参考にするといい.

* KickStart_Options_
* Select_Packkages_


How to use kickstart file?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

さて, 上記で作ったファイルをOSインストール前なのにどう使うのかという疑問がでるだろう.

OSのisoファイルを起動するとInstall選択画面が出てくる.
その画面で, [Tab] を押下するとedit optionなるものを与えることができる.
そこに対して, kickstartファイルのパスを入力してあげる.


そうするとKickStartが完了する.

.. =============================
.. リンク一覧
.. =============================

.. _KickStart_Options: https://access.redhat.com/documentation/ja-jp/red_hat_enterprise_linux/6/html/installation_guide/s1-kickstart2-options
.. _Select_Packkages: https://access.redhat.com/documentation/ja-jp/red_hat_enterprise_linux/6/html/installation_guide/s1-kickstart2-packageselection
