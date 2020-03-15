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

Introduction
==============

Developers of large-scale cloud applications seek...
1. better performance
2. lower management cost
3. higher efficiency
lead to 
1. Containerization
2. Remove Direct Memory Access (RDMA)

because
Container : lightweight isolation and portability,  lower deploy & manage cloud application

RDMA : higher throughput, lower latency, lower CPU utilization
deep learning and data analytics frameworks are adopt RDMA.

but, this two trend are fundamentally at odds with each other in cloud.

Container required three properties in networking
1. Isolation
have its dedicated network namespace
2. Portability
use virtual network
3. Controllability
orchestrators can easily enforce controle/data plane policy.
These properties are necessary for clouds.
To this end, in TCP/IP-based operations, networking is fully virtualized via software (virtual) switch

However, it's hard to fully virtualize RDMA-based networking...
difficult to modify the controle plane states (e.g. routes) in hardware in shared cloud environments, while it's also hard to control the data path since traffic directly goes between RAM and NIC via PCIe bus.

data-intensive application...
use RDMA only when running in dedicated baremetal cluster...


So, our goal is simple
we want cloud-base, containerized applications to be able to use RDMA + archiving the isolation, portability and controllability 

No mature (成熟していない) RDMA virtualization for container (table1. このあとのパラグラフでは内容がつらつらと書かれている。）
SoftRoCE => top of the UDP networking stack and use existing virtual IP networking solution... performance ✗

FreeFlow, a software-based virtual RDMA networking framework for containerized clouds, which simultaneously achieves isolation, portability, controllability, and performance (close to bere-metal)

FreeFlow's heart : software virtual switch running on each server to virtualize RDMA on commodity RDMA NICs.
This switch can full access to both control path(address, routing) and data path(data traffic) of the communications among containers.
this design phyrosophy is similar to existing software virtual switches used for TCP/IP networking in the containerized cloud. (eg. OvS)
differenct from OvS due to RDMA's characteristics

two key challenge
1. FleeFlow to be completely transparent to the application. RDMA requires a NIC to manipulate memory buffers and file descriptors....container can't directly intaract with NIC due to network virtualization.

if FreeFlow and container share the same memory (4.3) and file discriptor (4.4), any operation on the underlying physical RDMA NIC will automatically take effect inside the container(これ重要）

2. FreeFlow must offer throughput, latency.
identify the bottleneck(memory copy and inter-process communication respectively) and leverage a zero-copy design for throughput and a shared memory inter-process channel with CPU spining for latency.


Background
===========

Spark : mapper and reducer node is an individual container
Tensorflow : parameter server node, worker node is also an individual container
containers exchange data via network solution -> affect the degree of isolation and portability

many applications use virtual mode networking
network namespace of container : isolated
virtual IP : portable
can communicate via virtual(overlay) network composed by software virtual switches on host.
all data traffic must go through the virtual switch -> they have access to the traffic : controllable


table2 : performance improvement of using RDMA for deeplearning appliacation.(RNN example and description of the reason for improvement.)

Need for software-based RDMA virtualization
virtual mode networking : enhance isolation, portability, controllability.
RDMA : significant performance boost

Q. how do we use RDMA networking with containerized application that require virtual mode networking, especially in a cloud environment.

One possible approach to "virtualize" RDMA ... use hardware-based solution such as SR-IOV
-> limit the portability offered by the virtual mode networking.
SR-IOV, the NIC runs a simple layer-2 switch that marely perform VLAN forwarding. all packets (generated from and destined to a virtual network) have to be directly routed in the underlying physical network. 
if you want to migrate contianer C1 from Host1 to Host2 need to reconfigure the physical switch to route C1's packet to Host2.

so we believe that right approach to virtualize RDMA network for containers is to use a software switch. (Figure1 (b))
physical network  only deriver packets targeting on differenct hosts, and virtual networking routing is completely realized in software switches inside each host, which is independent with the physical network.
software switch can controll all addressing and routing, provide good isolation and portability for control plane.

Overview
===========

goal of FreeFlow : provide an virtual interface in-side each container, and applications can use RDMA via a virtual network on top of the virtual interface in an unmod-ified way

Ideally,  the performance of the virtual network should be close to bare-metal RDMA, and policies on both control and data path are flexible to be configured purely in software. 

system architecture and key challenges in this section

Overall Design
-----------------

Figure 2(a): native RDMA
application leverage RDMA API.
Freee Flow intercept the communication between app and NIC, and perform control plane and data plane policies inside the software FreeFlow router which runs as another container on the host machine.

for controlling data path, FreeFlow router only allow the NIC to directry R/W from it's own memory (shadow memory in Figure2 (b))
take the charge of copying data from and to the application memory

zero-copy : memory inside container <=> shadow memory in the FreeFlow router.


Verbs: the "narrow waist" for RDMA
------------------------------------

how to intercept the communication between app and physical NIC?
a lot of ways are exists, but we choise the efficient one.

using IB verbs API (verbs)

Verbs concept : "queue pairs (QP)" for data transfer
every connection ... two endpoints 'send queue (SQ)' and 'receive queue (RQ)' together called 'QP'

send queue : holds info about memory buffer to be send
receive queue : holds info about which buffers to receive the incoming data.
also has a separate completion queue (CQ) used the NIC to notify the nedpoint about completion of send or receive requests.

(なんかNVMeみたいだね)

Freeflow creates virtual QPs and CQs in virtual NICs and relates the operations on them with operation on read QPs and CQs in the physical NIC


FreeFlow architecture
-----------------------

Figure 4: the architecture of FreeFlow.
we modify or introduce tree components of container networking stack are shown in gray.

1. FreeFlow network library (FFL) (in the container)
   - key to making FreeFlow transparent to applications
   - (app perspective) indistinguishable from the standard RDMA verbs library (no modification)
2. FreeFlow software router (FFR) (in the host)
   - singnle instance on each host and works with all containers on the same host to provide virtual networking.
   - (In the data plane) FFR shares memory buffers, and isolate the shared memory buffers for different containers
   - send and receive data in the shared memroy through the NIC, relying on FFL to sync data between application's private data buffers and the shared memroy buffers.
   - impl data plane resource policies, (e.g, QoS) by controlling the shared-memory channel beween containers and FFR.
   - work with FFO to handle bookkeeping tasks such as IP address assignment
3. FreeFlow network orchestrator (FFO) (indepenedent from each host)
   - control-plane decision for all containers.

Challenges
------------

two key challenge

1. FreeFlow should provide RDMA interface which transparently support all types of existing RDMA operations. it is hard to support...
2. FreeFlow should provide near bere-metal RDMA performance while minimizing CPU and memory overhead.

present our approach for each challenge in section 4 and section 5 respectively

Transparent Support for RDMA Operations
==========================================

verbs supports multi type of operations and mechanism

one-side operation (WRITE/READ) / two-sided operaitions (SEND/RECV)

FreeFlow completely and dransparently support such different types of RDMA operation!!

(頑張ったねぇ)

Primary challenge : support one-sided operations and event-based completion notifications, in which RDMA NIC can modify memory or file descriptors in FFR silently.

(ちょっとこれイメージつかない。内容はちゃんと書いて有りそうだが。)

solve this challenge

1. container are essentially process, so FFL and FFR can share memory and file discriptor
2. physical NIC modifications can automatically be passed into containers.

but...

application inside container do not allocate memory in IPC shared memory space, so sharing memory between FFL and FFR is not straightforwared. need to convert the memory to shared memory transparently.


Connection Establishment
---------------------------

two RDMA communication endpoints need to first establish a connection.
they create a QP in each one's NIC, registering a bufffer of memory to the QP and pairing local OP with remove QP.
after establish connection, application can ask the NIC to send the content in the registered memory to the remote endpoint or put received data into the local buffer.

Figure5. steps1-7 show the typical process of connection establishment using verbs
left column : sequence of verbs calls made by the application
right column (2 column) : how FreeFlow traps the verbs call from the application, and to establish a connection between the sender's FFR and the receiver's FFR.

Step1
^^^^^^

Step2
^^^^^^^

Step3
^^^^^^^

Step4
^^^^^^^

Step5
^^^^^^^

Step6
^^^^^^^

Step7
^^^^^^^
