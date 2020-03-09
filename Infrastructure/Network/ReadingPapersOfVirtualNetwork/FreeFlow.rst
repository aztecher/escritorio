====================
FreeFlow
====================

Abstraction
=============

* large-scale cloud application using containerization
    a. high resouce efficiency
    b. lightweight isolation

but, on the other hand

* data-intensive application require high network performance such as RDMA

**inevitable collision course!!!**

*FreeFlow*
software-based RDMA virtualization framework desgined for containerized cloud.
not offloading, using commodity RDMA NICs.

already exists some approach, but FreeFlow is

* satisfies the requirements from cloud environment
    1. isolation for multi-tenancy
    2. portability for container migration
    3. controllability for control and data plane policies.
* low CPU overhead, and it's performance close to baremetal RDMA.
* In Tensorflow, Apache Spark, FreeFlow provides almost the same application performance as a baremetal RDMA.


containerized-cloudが盛り上がっている。
重要な観点として、リソース高効率、軽量なアイソレーションなど。
その一方のせるアプリケーションの視点としてはデータインテンシブなものが特に最近はある。（データ分析やディープラーニングなどの台頭による）
これらはRDMAなどのハイパフォーマンスなネットワークを要求する。
そのため、これらは互いにコリジョンしてしまう。

それを解決するためにFreeFlowwを提案する。
これはsoftware-based RDMA virtualization、つまりあえてRDMAの旨味を殺す（気がする）がContainerized環境でRDMAを利用できるようにするものにと見受けられる。
ただRDMA virtualizationといってもRDMA NICsは利用するため、物理的なものを仮想化に自由に取り回すという考え方とは少し異なるアプローチなきがする。
実際には通常のRDMAと同程度のパフォーマンスが出るらしいが本当か？


