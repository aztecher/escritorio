=====================
Trouble Shooting
=====================

Vagrantを使っていく上で鉢合わせたエラーに関して対処法を記述していく.

.. warning::

    エラー内容が表示されている場合は必ずそれを記述すること. また, 可能な限りエラー時の諸症状等を記述すること.


Vagrantfileに関する諸注意事項
---------------------------------

書き方という意味では, TroubleShootingに書くまでもないようなエラーが多いので省略するが, もうすこし細かくないレベルの注意事項を記述しておく.

VirtualBoxのバージョンに関する警告
------------------------------------------

症状
^^^^^^

``vagrant up`` 時に以下のようなエラーがでる. 時間が経つと無事起動する.

.. code-block:: sh

        default: SSH auth method: private key
        default: Warning: Connection reset. Retrying...
        default: Warning: Remote connection disconnect. Retrying...
        default: Warning: Connection reset. Retrying...
        default: Warning: Remote connection disconnect. Retrying...
        ......
        default: Warning: Remote connection disconnect. Retrying...
    ==> default: Machine booted and ready!
    ==> default: Checking for guest additions in VM...
        default: The guest additions on this VM do not match the installed version of
        default: VirtualBox! In most cases this is fine, but in rare cases it can
        default: prevent things such as shared folders from working properly. If you see
        default: shared folder errors, please make sure the guest additions within the
        default: virtual machine match the version of VirtualBox you have installed on
        default: your host and reload your VM.
        default:
        default: Guest Additions Version: 5.2.10
        default: VirtualBox Version: 5.1

考察
^^^^^^

Guest Additionのバージョンに対して, VirtualBoxのバージョンが足りていないようだ. そのため, VirtualBoxのバージョンをアップグレードするといいだろうと考えた.

解決方法
^^^^^^^^^^

``brew`` のリストで検索をしても引っかからなかった. ``brew cask`` のリストで検索すると見つかったので, これをアップデートすればいい.

.. warning::

    もしVMを起動していたら全て停止させた情況で行なうこと.
    "VBox..."という名前のプロセスが残っていても怒られた. 素直に ``kill -9`` でプロセスを殺して再度実行する.

.. code-block:: sh

    brew list | grep virtualbox
    > (結果なし)
    
    brew cask list | grep virtualbox
    > virtualbox (Hit)

    brew cask upgrade virtualbox

その後, もとのVMは削除し再度起動する.
さて, これでも初回起動時には ``insecure_key`` がどうのこうのという話がでてきた.
ただし, 一度停止して再度起動した際には特に問題なく起動できているようだったため, ひとまず解決とした.

備考
^^^^^^

ネットを調べるとどうも逆パターン(Guest Additionsのバージョンが古い)場合にも同様の問題が起きるようだ. この際には, ``vagrant-vbguest`` というVagrantのプラグインをインストール(``vagrant plugin install vagrant-vbguest``)し, ``vagrant vbguest`` でアップデートをすると解決するらしい.


XXX-cloudimg-console.log の出力抑制
------------------------------------------

症状
^^^^^^

``vagrant up`` を行なうと, ``XXX-cloudimg-console.log`` という名前のログファイルが作成される. 実害はないしログは重要だということは認識しているが, 現状使いみちが無いので出力の抑制を行なう.

考察
^^^^^^

あまり検討がつかないのでとりあえずググった. ただ, Vagrantfileでどうにかなるだろうという目算は立っていた(それ以外どうしようもないし...).

解決方法
^^^^^^^^^^

Vagrantfileに以下を追記すると抑制できるようだ. 理由はよくわかっていないがとりあえず良しとした.

.. code-block:: sh

    config.vm.provider "virtualbox" do |vb|
	  vb.customize ["modifyvm", :id, "--uartmode1", "disconnected"]
    end

