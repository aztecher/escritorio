==============
Preseed
==============

.. note::

    本来, PreseedとPackerは独立したツールであるが, Preseedに関しては現状Packerでの利用以外を考えていないため, このディレクトリに説明書きを載せておく.


PreseedはOSのインストールを自動化する仕組みである. :doc:`./KickStart.rst` と担う領域は同じであるといえる. ``preseed.cfg`` というファイルを作成し制御する.

Preseedを使うと言ってもインストール方法はいろいろある. それぞれメリデメがあるため, 必要な環境に応じて使い分けるのが無難.

1. ISOイメージに, ``preseed.cfg`` を同梱しオールインワンにする.
2. PXEブート方式. pxelinuxのイメージをTFTPで取得し, ``preseed.cfg`` をネットから取得して, それに従ってネットワークインストールする. DVDはいらないが, DHCPサーバーやTFTPサーバーの用意は必要.
3. 1で作成するISOイメージで, ``preseed.cfg`` のセッテイのみネットワーク上からHTTPで取得する方法. DHCPサーバーが必要.

今回はお試しとして, 1の方式で作成してみる. DHCPサーバーなどの環境をトト萌えたら2や3も試して記述しておくようにする.

.. note::

    ディレクトリやファイル名は適宜読み替える

オールインワン構成の手順
==========================

ISOファイルのマウントとコピー
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

基本的には, ISOの中身を取りだし加工して詰め直すというような作業になるため, ISOの取り回しが必要になる. ここでは少し, コマンドラインから取り回す方法について記述しておく.


まず, ISOをマウントしファイルをコピーする.
.. code-block:: sh

    $ sudo mkdir -p /mnt/iso
    $ sudo mount -o ro,loop ubuntu-16.04-server-amd64.iso /mnt/iso
    $ mkdir iso_root
    $ rsync -av /mnt/iso/. iso_root/.

iso_root/isolinux/isolinux.cfgの書き換え
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

timeout値を1に設定する.

.. code-block:: sh

    $ sudo vim iso_root/isolinux/isolinux.cfg
    > timeout: 1

iso_root/isolinux/txt.cfgを書き換える
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

古くはisolinux.cfgで多くの設定が行われていた(らしい)が, Ubuntu16.04では, txt.cfgを中心に書き換えることになるそうな. 内容全体を次のように書き換える.

.. code-block:: cfg

    default install
    label install
      menu label ^Install Ubuntu Server
      kernel /install/vmlinuz
      append file=/cdrom/preseed/preseed.cfg vga=normal initrd=/install/initrd.gz auto=true locale=en_US.UTF-8 console-setup/charmap=UTF-8 console-setup/layoutcode=us console-setup/ask_detect=false pkgsel/language-pack-patterns=pkgsel/install-language-support=false quiet --

設定に記述されている, ``/cdrom/preseed/preseed.cfg`` のようなパスは, 作成するISOイメージがインストーラーでは, ``/media`` にマウントされている前提で記述している.

iso_root/preseed/preseed.cfgを準備する
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

肝となる部分である. ただ基本的には人の設定をコピーして必要な部分だけ資料を参考にしながら編集する形になるだろう.

つまり, 以下は基本的にはテンプレとして考えてくれていい.

.. code-block:: cfg

    # ===================================================
    # Boot sequence configuration start
    # 後述する, Boot sequence configuration end
    # の設定のところまではDVDメディア, USBメディアに
    # 同梱している場合にのみ有効になる設定
    # PXEブートの場合はこのセクションは無視される.
    # この場合は, pxelinuxのconfigのappendに直接記入する
    # ====================================================

    # TODO: en, US を jpに変換するかどうかは試してから決める
    # NOTES:
    # 1. country = 'JP' locale = en_US.UTF-8 はエラー -> country = USとして解消
    d-i debian-installer/language string en
    d-i debian-installer/country string US
    d-i debian-installer/locale string en_US.UTF-8
    d-i localechooser/supported-locales en_US.UTF-98
    d-i console-setup/ask_detect boolean false
    d-i console-setup/layoutcode string us
    d-i console-setup/charmap select UTF-8

    # キーボードレイアウトの設定 (修正した)
    d-i keyboard-configuration/layoutcode string us
    d-i keyboard-configuration/modelcode us101

    # =======================================================
    # Network configuration
    # =======================================================
    #
    # 静的IP
    #
    #   preseed.cfgを外からもてこようとするとどうしても一旦
    #   DHCP解決をする必要がある.
    #   そして, 以下の netcfg 項目は一回目は無視されるので,
    #   d-i preseed/run のところでネットワーク設定をリセットする
    #   ハックが必要になる.
    #
    #   詳しくは以下:
    #   - https://help.ubuntu.com/lts/installation-guide/i386/preseed-contents.html
    #   - http://debian.2.n7.nabble.com/Bug-688273-Preseed-netcfg-use-autoconfig-and-netcfg-disable-dhcp-doesn-t-work-td1910023.html
    #
    #   以下の2項目を設定しないと静的IPとして処理されないので重要
    #
    # ---
    #
    # d-i netcfg/use_autoconfig boolean false
    # d-i netcfg/disable_autoconfig boolean true
    #
    # d-i netcfg/choose_interface select ens33
    # d-i netcfg/disable_dhcp boolean true
    # d-i netcfg/get_nameservers string 8.8.8.8 8.8.4.4 1.1.1.1
    # d-i netcfg/get_ipaddress string 192.168.1.201
    # d-i netcfg/get_netmask string 255.255.255.0
    # d-i netcfg/get_gateway string 192.168.1.1
    # d-i netcfg/confirm_static boolean true
    # d-i netcfg/get_hostname string stack01
    # d-i netcfg/get_domain string example.com
    # d-i netcfg/wireless_wep string

    #
    # DHCP
    #
    # ---
    #
    # d-i netcfg/choose_interface select ens33
    # d-i netcfg/disable_autoconfig boolean false
    # d-i netcfg/get_hostname string openstack
    # d-i netcfg/get_domain string sv.example.com
    # d-i netcfg/wireless_wep string
    #

    #
    # 一旦リセット
    #
    # ---
    #
    # d-i preseed/run string http://192.168.0.100/prescript.sh
    #

    #
    # ALL IN ONE
    #
    # ---
    #
    # NOTES:
    #   get_hostname, get_domain は必要に応じて修正する.
    d-i netcfg/choose_interface select auto
    d-i netcfg/get_hostname string unassigned-hostname
    d-i netcfg/get_domain string localdomain

    # =========================================================
    # Boot sequence configuration end
    # =========================================================

    # インストーラーパッケージをダウンロードするミラーを選択
    # d-i mirror/country string manual
    # d-i mirror/http/hostname string jp.archive.ubuntu.com
    # d-i mirror/http/directory string /ubuntu/
    # d-i mirror/http/proxy string

    d-i mirror/http/mirror select jp.archive.ubuntu.com

    # ここらへんなにしてるんでしょ?
    d-i apt-setup/restricted boolean true
    d-i apt-setup/universe boolean true
    d-i apt-setup/backports boolean true
    d-i apt-setup/services-select multiselect security
    d-i apt-setup/security_host string security.ubuntu.com
    d-i apt-setup/security_path string /ubuntu

    # インストールするSuiteの選択
    d-i mirror/suite string xenial

    # この辺は確認して変更する
    # NOTES:
    #   Passwordの入力・再入力を求められた
    # 解決策:
    #   passwd/user-password, passwd/user-passwordの入力
    #
    # NOTES:
    #   username, passwordの設定は適切に行なう
    #
    d-i passwd/user-fullname string Admin User01
    d-i passwd/username string ubuntu
    d-i passwd/user-password password temppwd
    d-i passwd/user-password-again password temppwd

    d-i user-setup/allow-password-weak boolean true
    d-i user-setup/encrypt-home boolean false

    # 時刻設定
    d-i clock-setup/utc boolean false
    d-i time/zone string Asia/Tokyo
    d-i clock-setup/ntp boolean true
    d-i clock-setup/ntp-server string ntp.nict.jp

    # ==========================================================
    # Partman partitioning section start
    # ==========================================================
    #
    # Partition Design
    #
    #   DiskSize: 80GiB
    #
    #   +------------+----------+-----------+
    #   | MountPoint | Min Size |  Max Size |
    #   +============+==========+===========+
    #   | /boot      |     1GiB |      1GiB |
    #   +------------+----------+-----------+
    #   | /          |    50GiB | unlimited |
    #   +------------+----------+-----------+
    #   | swap       |    29GiB |     29GiB |
    #   +------------+----------+-----------+
    #
    # Syntax
    #   <limits>::<minimal size>_<priority>_<maximum size>_<parted fs>
    #
    # NOTES:
    #   これに関しては場合により修正が必要.
    #   また, MBPでなく, GPTの場合(大容量HDDを取り扱う場合)設定を少し変える必要がありそう.

    # NOTES:
    #  Partition disksで止まった
    #  Partitioning methodを決めろとのこと.
    #
    #    Guided - use entire disk
    #    Guided - use entire disk and set up LVM
    #    Guided - use entier disk and setup encrypted LVM
    #    Manual
    #
    #  とりあえずManual選んだらだめだった
    #  そのため, 一番上のやつ選んだら, また選択画面 (DISKの選択?) を求められたので
    #  出てきたディスクを選んだ
    #
    # 解決策
    #  とりあえずいろいろtypeがあったのを修正した

    # 全てのRAIDデバイス構成を破棄
    d-i partman-md/device_remove_md boolean true
    # 全てのLVMデバイス構成を破棄
    d-i partman-lvm/device_remove_lvm boolean true
    d-i partman/confirm_nooverwrite boolean true

    # diskはsda一個
    d-i partman-auto/disk string /dev/sda
    d-i partman-auto/method string regular

    # パーティショニング
    d-i partman-auto/expert_recipe string \
          root ::                         \
            1024 10 1024 ext2             \
              $primary{ }                 \
              $bootable{ }                \
              format{ }                   \
              use_filesystem{ }           \
              filesystem{ ext2 }          \
              mountpoint{ /boot }         \
            .                             \
            51200 30 -1 ext4              \
              $primary{ }                 \
              method{ format }            \
              format{ }                   \
              use_filesystem{ }           \
              filesystem{ ext4 }          \
              mountpoint{ / }             \
            .                             \
            29696 20 29696 linux-swap     \
              $primary{ }                 \
              method { swap }             \
              format { }                  \
            .

    d-i partman-auto/choose_recipe select root
    d-i partman-partitioning/confirm_write_new_label boolean true
    d-i partman/choose_partition select Finish partitioning and write changes to disk
    d-i partman/confirm boolean true

    # =====================================================
    # Partman partitioning section end
    # =====================================================

    d-i base-installer/install-recommends boolean true
    d-i base-installer/kernel/image string linux-generic

    # NOTES:
    #   正確にどこの表示かわからないがとりあえずこの辺に記述
    #   Configure the package manager の HTTP Proxyの設定で止まった
    #   使わない場合 blank と書いてあるのでそうするだけでいいだろうが
    #   これのハンドリングも必要そう
    #
    # 解決策:
    # 
    d-i mirror/http/proxy string

    d-i apt-setup/use_mirror boolean true

    d-i debian-installer/allow_unauthenticated boolean true
    tasksel tasksel/first multiselect none

    # select additional install package
    # d-i pkgsel/include string openssh-server \
    #                           tcpdump        \
    #                           lvm2           \
    #                           curl wget      \
    d-i pkgsel/include string openssh-server
    d-i pkgsel/upgrade select none
    d-i pkgsel/update-policy select none
    d-i pkgsel/install-language-support boolean true
    popularity-contest popularity-contest/participate boolean false
    
    # GRUB Installer
    d-i grub-installer/grub2_instead_of_grub_legacy boolean true
    d-i grub-installer/only_debian boolean true
    d-i grub-installer/bootdev string /dev/sda

    # reboot
    d-i finish-install/reboot_in_progress note

    # NOTES:
    #   Select and install software
    #   なんかエラー出た.
    #   とりあえず以下, 文章抜粋
    #
    #   An installation step failed.
    #   You can try to run the failing item again from the menu,
    #   or skip it and choose something else.
    #   The failing step is: Select and install software
    #
    #  うーんなんかソフトウェアインストールでエラーしてるっぽいな.
    #
    # 検証1:
    #   とりあえず, pkgsel/include をopenssh-serverのみにしてみる.
    #   -> うまく行った. つまりコメントアウトしている中にそのままインストール出来ないものがありそう

isoイメージを作成する
^^^^^^^^^^^^^^^^^^^^^^

iso_rootの準備がこれで整った. あとは次のコマンドでISOイメージを作成する. ファイル名に日付を入れることで上書きをしないようにしている.

.. code-block:: sh

    $ sudo genisoimage -N -J -R -D -V "PRESEED.UB1604" -o ubuntu-16.04-server-amd64-preseed."$(date +%Y%m%d.%H%M%S)".iso -b isolinux/isolinux.bin -c isolinux/boot.cat -no-emul-boot -boot-load-size 4 -boot-info-table iso_root/


