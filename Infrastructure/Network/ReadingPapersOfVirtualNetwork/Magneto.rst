============
Magneto
============

Abstraction
=============

Problem
=========

OpenFlowを基軸としたProgrammable Switchの普及。
ただし現状のenterprise switchをすべてそれに移行するのはコスト的に現実的ではない。
問題は、コストがかかる割には使い方によっては対効果があがらない場合があること。
そもそもかかるコストが大きすぎること。
（ほかあったっけ）
これを緩和する方法はいくつか考えられて来たが、基本的にはどれも物理的にswitchを触る必要があり、コスト軽減はできても現実的なところまでコストが抑えられていない状況である。
今回のmagnetoは現在存在しているlegacy switch (OF機能のないswitch)に手を加えることなく, いくつかのswitchをOF機能ありのものにreplaceするだけで、すべてのswitchをOFに変更した場合と同等のcontrollができるようなネットワークを構成することができるというもの。
実験してみたところ、全体の約20%をOF機能ありのものに変更するだけで上記のような効果が得られたらしい（そこまで読んでない）

Background (with Related works) and Motivations
-------------------------------------------------



Assumptions
=============

いくつか前提にしているところがある。
1. hybrid networkの前提であり(legacy switch + programmable switch), central SDN controllerからprogrammable switchをコントロール可能であるが、直接switchのforwarding entriesを更新できない状況。
2. legacy switchは、legacy networkを構成するために利用していたMAC addressの学習設定が起動しており、Spanning Tree Protocolなどによりloop回避がされていること。

このような前提でMacnetoは以下のようなことができる。
1. あるPath（P）が形成されているものに対し、別のPath(P')になるように張り替える。このときlegacy switchに対して直接オペレーションすることも、新規ソフトウェアのインストールなどもしない。
2. magnetoによりlegacy switchのforwarding entriesを書き換える


Magneto Mehanism
===================

Magnetoは大きく分けて２つのメカニズムからなる。 ``telekinesis`` と ``magnet address`` である。

telekinesis
--------------

OpenFlow switchからlegacy switchに対して ``seed packet`` を注入する仕組みのことを ``telekinesis`` と読んでいる。 ``seed packet`` はcustom packetであり、このpacketによりlegacy switchのforwarding tableが書き換わる。

(そもそもOpenFlowでカスタムパケットとか投げれるんか?)
(OpenFlowに関するpageをつくるべきだろう)


Short Commings of Baseline Telekinesis
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

(これここに書くかファイルを分けるか考える)


magnet addresses
------------------

``magnet address`` とはARP messageの際に、エンドホストのARP cache tableに差し込む「架空の」MAC addressである。 ``magnet address`` によりnetwork visibilityの獲得やエンドホスト、legacy switchのforwarding behaviorのコントロールを行うことができる。

example
--------


Magneto path control components
===================================

