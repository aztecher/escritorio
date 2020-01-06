===================
Examples
===================

PackerのExampleを示しながら各動作に関して説明する.

いろいろなプロバイダ向けに作成できるため, 今回は現状一番利用の可能性がある Virtualbox (Vagrant) 向けのboxファイルの作成を通して説明していく.

本来, JSONファイルにはコメントアウトを喜寿捨つすることができないが, ここで示すExampleは説明のために'#'始まりをコメントアウトとして擬似的にコメントを記述していく.


Packer実行ファイル(Virtualbox)
=================================

.. code-block:: json

    {
      "builders": [
        {
          # 何の仮想インスタンスを作成するかを指定する項目.
          # AWSのインスタンスなども可能.
          # 今回はvirtualboxなので, 「virtualbox-iso」を記載
          "type": "virtualbox-iso",

          # デフォルトのvm名.
          # Vagrantでもオーバーライドできるため記載がなくても問題なし
          "vm_name": "box",

          # 最初にVirtualboxがbootするまでの待ち時間
          "boot_wait": "10s",

          # Virtualboxのディスク容量を指定
          "disk_size": 512000,

          # CentOSx86系なら, 「RedHat_64」を指定.
          # typeの一覧は 「VBoxManage list ostype」 でわかる
          "guest_os_type": "RedHat_64",

          # isoのchecksum.
          # わからない場合は, 「md5 作るOS.iso」でわかる
          "iso_checksum": "4a5fa01c81cc300f4729136e28ebe600",

          # isoのDL先のURL
          # HTTPしかサポートしていないよう?
          "iso_url": "http://ftp.iij.ad.jp/pub/linux/centos/6.4/isos/x86_64/CentOS-6.4-x86_64-minimal.iso",

          # sshユーザー名.
          # vagrantと連携したいならユーザ名はvagrantにしたほうが楽
          "ssh_username": "vagrant",

          # sshユーザのパスワード
          "ssh_password": "vagrant",

          # sshポート番号
          "ssh_port": 22,

          # sshの接続時間
          "ssh_wait_timeout": "10000s",

          # シャットダウンコマンド
          # haltコマンドを利用 (halt -p = shutdown -h now)
          # (要コマンド確認 - vagrantをパイプで渡しているのはなぜ...?)
          "shutdown_command": "echo '/sbin/halt -p' > shutdown.sh; echo 'vagrant' | sudo -S sh 'shutdown.sh'",

          # Virtualboxのguest_additionsのパス
          "guest_additions_path": "VBoxGuestAdditions_{{.Version}}.iso",

          # Virtualboxのバージョン番号
          "virtualbox_version_file": ".vbox_version",

          # Virtualboxのインスタンス設定
          # 今回はメモリを2G, CPUコア数を2としている
          "vboxmanage": [
            [
              "modifyvm",
              "{{.Name}}",
              "--memory",
              "2048"
            ],
            [
              "modifyvm",
              "{{.Name}}",
              "--cpus",
              "2"
            ]
        ],

        # インストール時にローカルにwebサーバーをたてて,
        # kickstartなどの設定情報を持っていくための設定.
        # kickstartはOSのインストールを自動化する仕組み
        #   * 言語は何にする?
        #   * ポスト名は何にする?
        #   * パッケージはなにをいれる?
        # というのを選択していく部分の自動化である.
        "http_directory": "./packer/builders/",

        # boot字のkickstartを実行するときの,
        # ks.cfg の場所を指定する.
        "boot_command": [
          "<tab> text ks=http://{{.HTTPIP}}:{{.HTTPPort}}/ks.cfg<enter><wait>"
          ]
        }
      ],

      # インスタンスが作成された後に実行するもの.
      # kickstartで仮想インスタンスにプラスアルファの機能を追加する場合
      # このprovisioners側で管理するといい.
      "provisioners": [
        {
          "type": "shell",
          "scripts": [
            "packer/provisioners/base.sh"
            "packer/provisioners/vagrant.sh"
            "packer/provisioners/virtualbox.sh"
            "packer/provisioners/cleanup.sh"
          ],
          "override": {
            "virtualbox-iso": {
              "execute_command": "echo 'vagrant' | sudo -S sh '{{.Path}}'"
            }
          }
        }
      ],
      "post-processors": [
        {
          # vagrantのボックスを作る場合はvagrantで
          "type": "vagrant"

          # vagrantのボックスを作った後の,
          # .boxの吐き出し場所の指定.
          "output": "./packer/vagrant-boxes/CentOS-6.4-x86_64-minimal.box"
        }
      ]
    }


builders
-----------

まず, ``builders`` である. これは作る仮想インスタンスの設定を記載する領域になる. それぞれの意味は上記コメントアウト参照のこと.

provisioners
--------------

インスタンスが作成された後に実行するもの. KickStartで作成した仮想インスタンスにプラスアルファの機能を追加する場合は, このprovisioners側で管理するほうがいい.

post-processors
-----------------

名前の通り, OSを作成した後に実行することを記載する項目. それぞれの意味は上記のコメントアウト参照のこと

KickStart
===========

Kickstartに関しての詳細は :doc:`./KickStart.rst` を参照されたし.

今回は次のようなファイルを作成

.. code-block:: sh

    # まずはディレクトリ作成
    mkdir -r packer/vagrant-boxes packer/builders
    vim packer/builders/ks.cfg

.. code-block:: cfg

    install cdrom
    lang en_US.UTF-8
    keyboard us
    network --bootproto=dhcp
    rootpw vagrant
    firewall --enabled --service=ssh
    authconfig --enableshadow --passalgo=sha512
    selinux --disabled
    timezone Asia/Tokyo
    bootloader --location=mbr

    text
    skipx
    zerombr

    clearpart --all --initlabel
    autopart

    auth --useshadow --enablemd5
    firstboot --disabled
    reboot

    %packages --ignoremissing
    @core
    gzip2
    kernel-devel
    kernel-headers
    -ipw2100-firmware
    -ipw2200-firmware
    -ivtv-firmware
    %end

    %post
    /usr/bin/yum -y install sudo
    /usr/sbin/groupadd -g 501 vagrant
    /usr/sbin/useradd vagrant -u 501 -g vagrant -G wheel
    echo "vagrant" | passwd --stdin vagrant
    echo "vagrant   ALL=(ALL)   NOPASSWD: ALL" >> /etc/sudoers.d/vagrant
    chmod 0440 /etc/sudoers.d/vagrant
    %end

ビルドの実行
===============

記述したjsonファイル(packer.json)が存在するディレクトリで次のコマンドを実行する.

.. code-block:: sh

    packer build --only=virtualbox-iso packer.json

これで指定した場所に, boxファイルできる.

Vagrantfileに追加
===================

Vagrantfileがない場合は作成する. 今回の場合は, 例えば以下のようになる

.. code-block:: rb

    BOX_NAME = "CentOS6.4-x86-minimal"

    Vagrant.configure("2") do |config|
      config.vm.box = BOX_NAME

      config.vm.box_url = "./packer/vagrant-boxes/CentOS-6.4-x86_64-minimal.box"

      config.vm.network :private_network, ip: "192.169.56.101"

      config.vm.provider :virtualbox do |vb|
        vb.name = BOX_NAME
        vb.customize["modifyvm", :id, "--memory", "2048"]
        vb.customize["modifyvm", :id, "--cpus", "2"]
      end
    end

これで,

.. code-block:: sh

    vagrant up

これで自作boxが作成されるはずである.


.. =========================
.. リンク一覧
.. =========================
