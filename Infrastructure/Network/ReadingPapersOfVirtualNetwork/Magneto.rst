============
Magneto
============

本記事は前提知識として、 ARPの詳細知識を要求する( :doc:`../ARP/Specification` )

また、本論文中で利用されるメカニズムとして、:doc:`./Telekinesis` を取り扱う。

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

Telekinesis単体だけでは、２つの欠点が存在することがわかっている。

**Coarse-grained paths**
path controlが粗雑になってしまうという欠点である。
これは、Legacy network L2ルーティングが、宛先ベースのルーティングであることに起因している。
宛先MACは各スイッチの単一interfaceに関連付けられるため、オペレータはその中でpath diversityを担保するためにオペレーションコストを増やして、VLANやECMPを利用している。(耳が痛い)
対して、OpenFlowの場合は、宛先と送り先MACアドレスともとにしたトラフィックベースのより繊細なpath controlを行うことができるわけである。
Telekinesisはどうしてもlegacy networkの制約を受け継いでしまうことになる。つまり、あるpath updateをトリガには同じ宛先に到達するすべてのpathがupdateされる(この辺はちょっと勉強しないとわからない)
Figure1に例を示す。この図において、H1とH4はともにH3に対してtrafficを流す。
ここでもしH1とH3の間のpathを(LE1, LE6, OF7, LE5)と変更すると、H4からH3へのパケットを含め、H3宛のすべてのパケットをOF7に転送することになる。
(この辺の知識はない。。。理解しなければ)

**Unstable paths**
(MACの学習の仕組みとかもわからんが。。。)
MACの学習は ``seed packet`` であろうとそうでなかろうとどんなパケットに対しても働く。
MACアドレス ``m`` の転送エントリーは、スイッチが ``m`` からのpacketをリレーするたびに変更されうるということ。
これにより最も単純なpath updateでさえ不安定になる可能性がある。
理解するために一般的なあるシナリオを検討してみる。それは、Figure2に示すように、２つのホスト間で双方向にtrafficが流れている状況で、TCP communicationをしているような場合である。(非常に一般的だと思われる)
Path updateが十分早くない場合、つまりまだpathとしてはPであるが、P'をpathとするようなpacketはforwarding entryのupdateを無効にし、Pの状態に戻す。
(これもいまいちよくわからない。TCPだからかな？やっぱりpath updateの仕組みがわからないのが地味に足を引っ張っている)

reverse trafficが存在するときのこの問題の最も単純な解決方法は、 forwarding entryがstable stateになるまで ``seed packet`` を継続的にinjectすることである。
つまり、seed packetがdata packetより早く付けば、updateは行われるみたいな雰囲気。

ただ、これを小さいケースではあるがリアルワールドでテストした構成がFigure3である。説明はTable1のちょい下に書いてあるので参考にして図に起こす。
成功レートを算出したのがTable1である。細かい試行回数などは上記と同じ位置を参照されたし。

これから、legacy switch間のdata packetが多ければ多いほど、path updateはうまく行かないということ。場合によってはupdateの見込みすらないということ。更にはoverheadが非常に大きいことなどがわかった。

magnet addresses
------------------

``magnet address`` とはARP messageの際に、エンドホストのARP cache tableに差し込む「架空の」MAC addressである。 ``magnet address`` によりnetwork visibilityの獲得やエンドホスト、legacy switchのforwarding behaviorのコントロールを行うことができる。


Magneto + Telekinesis
----------------------

Telekinesisでは、そのままだと使えない感じがわかったかと思われる。ただし、これと ``magnet address`` を組み合わせることによって、fine-grainedなpath controlや、visibilityの向上、ホストあいだのアクセスコントロールの強化など色々な利点をきょうじゅすることができるようになる。


Magneto path control components
===================================

重要なのは、MAC Address Leariningの機能(L2 SW)とARPによるMACの学習(L2/L3)は全く別ものということ。
MAC Learningに関しては気持ちとしては、意図的に行うものではなく、packetが流れ着いたときに受動的に行われるものだというイメージ。
対して、ARP requestはTCP/IPで通信するときなどに積極的に利用される。そうじゃないと通信できないから。
そのタイミングでbroadcastなどが行われるが、これはARP Protocolによるもの（カーネル内の機能）
それによってpacketが流れたりするわけなんだが、そのような流れの中でL2はL2でMAC Address Learningを行うし(MAC Address Tableを更新するし)
ARP ProtocolはARP Tableを作成／更新するイメージ
つまり完全に別モンなのだという認識.

ただしARP リクエストはEthernetIIだとかいうフレーム形式(L2)に準拠しているので、MAC Address Learningも機能するみたいな気持ちでいいと思う。
ARPリクエストの際のEthernetフレームにmagneto MACがどうこう。。。みたいな話が出てくるのでそこをちゃんと読めれば良さそう。

