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

(Step5, 6あたりがいまいち)

Step1
^^^^^^

queries the list of NICs whose drivers support verbs.
FFL intercept the call and returns the context data object of the virtual NIC of the container.

Step2
^^^^^^^

create QP and CQ on its virtual NIC (step1 return) while FFR creates the corresponding queues (QP' and CQ' / physical NIC queue virtualization). return the ID and meta-data of QP' / CQ'.

Step3
^^^^^^^

register a block memory(mem) to the QP. FFR allocates a corresponding block memory (s-mem) in its shared memory inter-process communication (IPC) space with the same size as mem, registers s-mem to QP'.
FFR returns the ID it used to create s-mem. FFL remember the tuple (QP, mem, s-mem, keys(ID)). FFL can map s-mem into its own virtual memory space with this ID.

Step4
^^^^^^^

queries the address (so-called GID in RDMA) of the local QP.
FFR returns the actual CID of QP'
so container obtain the routable address GID of the QP

Step5
^^^^^^^

exchange CID and OP-ID with the remove endpoint.
application can exchange this info via any channels such as TCP/IP or RDMA-CM.

Step6
^^^^^^^

pair with remove QP with the remote container's QP using the receiver's CID.
FFL forwards this CID to FFR. FFR pairs QP' with this GID.

Step7
^^^^^^^

modifies the state of local QP to Ready to Send/Receive state, while FFR modifies the state of QP' accordingly.

after then, from app perspective, ready to send or receive data.
-> created QP and CQ
-> registerd mem to the QP
-> paired with the remove QP and established a connection with the remove QP

from FreeFlow perspective, created 
-> created QP' and CQ' associated with the QP and CQ in the app
-> registerd s-mem as the shadow memory of me. paired with the QP' in the remote FFR.

FreeFlow may increase the latency for connection establishment
(additional interactions between FFR and FFL)
however, it does not much affect the overrall latency of FreeFlow because it's one-type cost ... many RDMA apps re-use pre-established commections for communication.

Two-sided Operations
-----------------------

sendar and receiver needs to go through two steps to perform a data transfer

1. use QP to start sending or receiving data.
2. CQ to get completion notification

Step8
^^^^^^^

the app invokes the SEND call, and supplies pointer to mem
FFL copy data from mem to s-mem
FFR invoke its own SEND call to send s-mem to the remote FFR
avoid memory copy from mem and s-mem -> zero-copy mechanism (section 4.3)

remote router would have posted a corresponding RECV call by this time.

Step9
^^^^^^^

the app either polls the CQ or waits for a notification that indicates the completion of the send.
FFR polls/waits-on CQ' associated with QP' and forward it to FFL

subsequent SEND operation on the same QP, the app only need to invoke Step8, 0 repeatedly.
FFL will copy data from s-mem to mem after OP' finish receiving data.


One-sided Operation
--------------------------

In one-side operation, client need not only the GID of a server, but also the address of the remote memory buffer and security key for accessing the memory.
This info is exchanged in Figure5's Step5 and be available to FreeFlow in Step8

more challenging to transparently suppport one-side operation.
two probleems to support one-sided operation in FreeFlow

1. target memory address 'mem' is the virtual memory of the remote container. but, the local FRR doesn't know the corresponding s-mem on the other side.

(ex.)
in Figure6(a), when the sender tries to write data in mem-1 to remote memory mem-2 h, it failes at stage 3 because the target memory address mem-2 is not accessible for FFR on the receiver side.


2. even if we know the memory mapping on the remote side, WRITE and READ can remotely modify or copy data without notifying the remote side's CPU, so that FFR does not know when to copy to or from application''s memory.

(ex.)
in Figure6(b), sender get the address of s-mem-2 and send the ata to it, but after the data is avaliable in s-mem-2, there is no notification for the FFR in the receiver side to know when to copy s-mem-2 to mem-2


to address this, in FreeFlow, we design a zero-copy based memchanism o efficiently support one-side operations.
high level idea : mem and s-mem the same physical memory, so that FFR does not need to do any copy, and the data will be naturally presented.

Figure6 (c) illustrates the design.

key : make applications directly allocate and use shared memory with FFR for data transfer.
For this, FreeFlow provides two options,

Operation1 - Allocating shared buffers with new APIs.
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

create new two verbs functions, `ibv_malloc` `ibv_free` to let applications delegate the memory creation and deletion to FreeFlow.
This allows FFL to directly allocate these buffers in the shared memory region and thus avoid the copy.

(need to modify application code, but modification should be only several lines)


Option2 - Re-mapping application's virtual memory address to shared memory
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

(ちょっとここ曖昧)

Step3, the app registers a private memory piece with virtual memory addres 'va' as a data buffer, FFL release the physical memory piece behind 'va' and assign a shared physical memory piece from FFR to va.

In Linux, only valid when 'va' is an address at the start of memory page.
so, app to allocate memory always at the start of a page, FFL intercepts the call like `malloc` in C language and make it always return page aligned memory address.

this option can achive zero-memory-copy without modifying application code, it force all memory allocation in the app to be page aligned, which can result in lower memory efficiency on the host.


actualy, recommend the first option
but RDMA application alread make their data buffer page aligned for better performance (RDMA-spark) we can directly use the Option-2 without intercepting `malloc`, so the side-effect is limited.

FreeFlow will not incur any overhead in actual memory usage.


Event-based Operation
---------------------------

two option to get notified from CQs(Completion Queue)

1. let application poll the CQs periodically to check whether there are any completed ops
2. event-based. app creates an event channel and add CQs into the channel. channel contains the fd which can trigger events when operations are completed.

In FreeFlow, raw fd is created from physical NIC
FFR needs to pass fd to FFL
take advantage of the fact .. FFL and FFR are essentially two processes sharing the same OS kernel and leverage the same methodology to pass fd between processes (event channel)

Communication Channel between FFL and FFR
============================================

FreeFlow intercepts every verbs call via FFL -> FFR -> physical NIC

it need to have an efficient channel between FFL and FFR to provide high RDMA performance

this section show two design of such communication channel

5.1 Verbs Forwarding via File Descriptor
-------------------------------------------

A straightforwared way to pass verbs call FFL -> FFR ... RPC
FFL pass API name and parameters to FFR, and FFR exeute the API and return the result to FFL
but this approach doesnt work well in FreeFlow

why?
complexity of imput data structures of the verbs call (Figure 7(a))
typical function (example) in verbs `ibv_post_send` has the pointer of complex data structure.
FFL and FFR are in two different processes, the pointer of FFL will be invalid in FFR.

deep copy the structure between FFL and FFR?
1. data structure of verbs are quite deep so it hurt the performance
2. there are customized data structure -> deep copy cannot be predefined by FreeFlow

to address this issue (Figure 7(b))
verbs library consists of three layers.
complicated ... top layer (describes above)
simple enough (no pointer) ... middle layer, bottom layer (communicates with the NIC file descriptor)


forward the requests to be made for the NIC file descriptor! (not verbs function)

we replace 'the NIC file descriptor in the container' with 'a Unix socket file descriptor whose the other end is FFR' (Figure 7(c))

By doing this, FFR can learn the command sent by the application and the supplied parameters (???)
FFR will map the operations to virtual queues in the container to the same operations to the actual queues in the physical NIC
(replies (physical NIC -> virtual NIC) as the same via the Unix socket)

The NIC driver communication layer in FFL will process the reply normally without knowing about operations behind the Unix socket file descriptor.

unix socket based approach consume little CPU -> incur additional latency
measure -> round trip time over Unix socket can easily be >= 5 microsecond in the commodity layer -> can become a performance bottleneck for latency sensitive application.

For application requiring low latency communication, we will describe the design of Fastpath, which optimize the communication delay by trading CPU resources 

5.2 Fastpath between FFL and FFR
-----------------------------------

to accelerate the communication between FFR and FFL -> Fastpath
Fastpath in parallel with the Unix socket based channel between them.

Figure8
FFL and FFR co-own dedicated piece of shared memory
with Fastpath, FFR spins on a CPU core and keeps checking whether there is a new request from FFL got writen into the shared memory piece

request is detected, FFR will immediately execute it, FFL start to spin on a CPU core to check wheter the response is ready.
after reading the response, FFL will stop the CPU spinning on its side.

(8.1.2) Fastpath can significantly reduce the latency but the price is the CPU cycles spend on spinning for reading requests and response (cpu pinning?)

to limit the CPU overhead brought by Fastpath...

1. FFR only spins on one CPU core for all Fastpath channel with FFL on the same host.
2. Fastpath is only used for functions which are on data path and are non-blocking, so that the CPU spinning time on FFL to wait for a response will be short (few microsecond)

therefore
* significantly shorten the latency of message passing
* if FFO knows there is no latency sensitive application on a host machin (according to running container image) it can disable Fastpath and the CPU spinning


6 Implementation
==================

FFL ... modifying `libibverbs`, `libmlx4`, `librdmacm`. add 4000 lines of C code to implement FreeFlow's logic.
FFR ... from scratch in about 2000 lines of C++ code.
FFO ... use ZooKeeper to store the user defined information (e.g, IP assignment, access control, resource sharing policies, memory mapping information (one-sided operation))


Due to spece limits, we only show three representative implementation details next.

Control & data plane policies
--------------------------------

FreeFlow can control both control and data plane operations requested by containers
suppoot common control and data plane policies

* bandwidth enforcement
* flow prioritization
* resource usage enforcement

Example of control plane policy (in our (author's) properties)
FreeFlow enforce a quota for the number of QPs each container can create
(large number of QPs is performance degradation of RDMA NICs)
-> prevent a container from creating too many QPs which can impact other containers on the same host machine

Example of data plane policy
FreeFlow enable per-flow rate limiting with little overhead.
implement simple token-bucket data structure in FFR.

when app create a new QP
-> we check the policies (stored in FFO)
-> associate token-bucket with pre-set rate limit to the QP.

the route check wheter the QP has enough tokens to send out the requested message size
T -> send request is forwareded to real NIC
F -> notify FFL and delay it until there are enough token
(example only an example of implementing QoS policies)

FreeFlow provides flexible APIs for implementing sophisticated QoS algorithms in FFR


Memory management in Fastpath
----------------------------------

use assembly to flush the cache line data (written by FFL and FFR) to main memory immediately
because CPU will keep the newly written lines in cache for a while to wait more written lines, slowing down the message exchanging speed on Fastpath


Supporting parallelism
------------------------

application create multi QP and use multiple threads to transfer data in parallel, each unix domain socket between the FFL and FFR need a lock.

to implovem performance, create multiple Unix domain socket between FFL and FFR

avoid "head of line blocking"
dedicated more of these socket to 'data plane operation' and 'event notifications'
and dedicated only a few sockets to 'creation, setup and delete operation'

on FFR, use dedicated thread for each incoming unix domain socket connection, also create data structure for each container and dedicated shared memory region for each registered memory buffer to keep the data path lock free.


Discussion
============

CPU overhead
-------------

use CPU core for polling control messages between FFL and FFR to support low latency IPC channel.(Section 5.2)

this is a cost!

one possible approach to address this is to utilize hardware
-> offloading CPU tasks such as FPGA, ARM coprocessor, RDMA NICs

feature work

Security
---------

1. FFR share its memory with containers. one container can read by scanning the IPC space?
-> FFR create a dedicated  shared memory buffer for each individiual QP. not a concern

2. security of the memory keys. if one can see it by wiretapping, subsequent communications can be compromized.
-> this problem is one-sided operation in raw RDMA work, not made worse by FreeFlow

Work with external legacy peers
---------------------------------

containers in FreeFlow can naturally communicate with external RDMA peers.

Container migration
---------------------

FreeFlow supports offline migration naturally.
if the contianer migrate to another host, its IP address is not changed so that its perrs re-establish RDMA connection.
doesn't live migration, since RDMA has poor mobility nowadays

VM host
----------

if you can use SR-IOV enviromnent, you can use FreeFlow from container in VM host.

Congestion control
-------------------

RDMA NICs already have congestion control mechanisms, and FreeFlow relies on them

Evaluation
===========

1. microbenchmarks
2. performance of real-world applications on FreeFlow

Microbenchmarks
-----------------

Setup
^^^^^^

run microbenchmarks two testbeds

1. InfiniBand
- CPU : 2 Intel Xeon E5-2620 2.10GHz 8-core CPU
- RAM : 64GB
- NIC : 56Gbps Mellanox FDR CX3 NIC
- OS  : Ubuntu 14.04 with the kernel version 3.13.0-129-generic

2. RoCE
- CPU : Intel Xeon E5-2609 2.40GHz 4-core CPU
- RAM : 64BG
- NIC : 40Gbps Mellanox CX3 NIC
- OS  : Ubuntu 14.04 with the kernel version 4.4.0-31-generic
- ToR : Arista 7050QX

Common Configuration
Container Runtime - Docker (v1.13.0)
TCP/IP virtual network (for Container) - Weave (v1.8.0) with OvS kernel module enabled.
Fastpath enabled FreeFlow (unless otherwise specified)


main comparison is FreeFlow vs bere-metal RDMA
- minimal performance penalty

also demonstrate the performance of translating TCP socket calls into RDMA on top of FreeFlow (8.1.4)
there we also compare FreeFlow with bare-metal TCP and Weave

Throughput and Latency
^^^^^^^^^^^^^^^^^^^^^^^^^

focus on two performance metrics 'throughput' and 'latency'

use the benchmark tool : perftest (provided by Mellanox)
`ib_send_lat` and `ib_send_bw` for measure latency and throughput of two-sided operation(SEND)
`ib_write_lat` and `ib_write_bw` for one-sided operation (WRITE)

those tools can run on FreeFlow without anymodification (4.3)


inter-host performance value

Throughput
+++++++++++

measure the single thread RDMA SEND/WRITE throughput on two testbeds (Figure 9)
each run transmit 1GB data with different size (ranging from 2KB to1MB)

if message size >= 8K then FreeFlow gets full throughput as beremetal RDMA (46.9Gbps on Infiniband, 34.5Gbps on RoCE)

increase the number of container pair (flows) to up to 512, aggregated throughput of all flows is close to optimal (Figure 11)

also verify that bandwidth is fairly distributed among different flows
(by calculating Jain's fairness index, 0.97 on average)


In general, bandwidth-hangry application tend to use larger message size.
FreeFlow will have no throughput penaltiy in this case (8.1.2 CPU overhead)

even when message size = 2KB then FreeFlow still achives more than half of the full throughput.
the throughput is bounded by the single FFR Fastpath threading.
you want to remove this bottleneck, you assigne one more CPU and you can achive.
(balancing RDMA request more fast, the bottleneck removed!)

(なんか気になることが書いてあった。普通こんだけ小さいとLatencyの方を気にするよね的な？)

Latency
++++++++

measure the latency of sending 64B, 256B, 1KB, 4KB message.
two containers run on different hosts connected via the same ToR switch.
for each message size, we measure the latency 1000 times and plot the median, 10- and 99th-percentile latency value.

Figure 10.
one-sided WRITE operation have lower latency than two-sided SEND operation, and also smaller gap betweeen FreeFlow and bere-metal RDMA.
even with two-sided operation, less than 1.5 micro second extra delay!!
(the mainly due to the IPC between the FFL and FFR)

one-sided operation will trigger IPC only one time.
two-sided operations will trigger two times and one time memory copy.
-> larger latency gap of two-sided operation


compare values

network one hop (a hardware switch) = 0.55 micro second latency
-> FreeFlow latency overhead is comparable to an extra switch hot in the network.

host TCP stack latency is at least 10 micro second (8.1.4) and then TCP/IP virtual network latency is even larger (more tha 40 micro second in our test).
-> FreeFlow preserves the latency advantage of RDMA while enabling virtual network for containers.

CPU Overhead and Trade-off
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

FreeFlow has two modes (Fastpath / non-Fastpath (LowCPU))
default, Fastpath is enabled and provides the best performance in terms of latency.

In this mode,
FFR spins on one CPU core and serve Verbs requests as soon as possible.
One CPU core is capable of serving all the containers on the host, thanks to the fact that FFR only handles message-level events, instead of at packet-level like in OvS.

on a commodity server with many CPU cores, this is acceptable.


user may choose LowCPU mode.
which uses a Unix socket as the signal mechanism instead of core spinning.
-> hurt latency performance (increase 2.4 (FastPath) to 17.0 (LowCPU) micro seconds, Table 3)

Figure 12
record the per-process CPU utilization when measuring the inter-host throughput.
the throughput of all three cases in the figure are the same (full bandwidth)

(これイマイチ微妙。CPUのutilizeなのでLowCPUだとちゃんとスケジューリングしてるみたいなこと？)

recommended choosing the mode according to the work-load requirement
(これ、ごちゃまぜにできるんかな。。。できそうやな。ホスト単位で分ければ良い気がする)
latency-sensitive or non-CPU heavy (e.g GPU-heavy) application -> Fastpath mode

Rate Limiter and Performance Isolation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

demonstrate the performance of rate limiter mentioned in section 6 (Implementation)
Figure 13,
start a single flow between two containers on different hosts, on Infiniband testbed
-> limit the flow rate and set different bandwidth caps from 1Gbps -> 40Gbps.
-> we seed that the controlled bandwidth (y-axis) is close th the bandwidth cap we set (x-axis). FreeFlow archives this with only 6% CPU overhead.

FreeFlow can isolate performance for different containers using the rate limitter
-> 10 concurrent flows between container pairs and applied the different rate limit to each flow (from 1Gbps to 10Gbps)
--> verified that the throughput of each flow is accurately capped!!

TCP socket over RDMA
^^^^^^^^^^^^^^^^^^^^^^^

Enabling virtual RDMA can also benefit the performance of socket-based application (なぜ？)

show that FreeFlow provides better performance than conventional TCP/IP virtual network with the help of `rsocket` (existing socket-to-verbs translation layer)

in Infiniband and RoCE, by dynamically linking with `rsocket` during runtime, application socket calls are transparently translated into RDMA verbs calls.

run iperf for measuring TCP throughput and NPtcp for TCP latency `without any modifications on these tools.`
compare ageinst the same tools running on the virtual and host mode network

Figure14
latency, FreeFlow always outperformances weave. especially for small message latency, FreeFlow is consistently lower than even host TCP/IP, bu up to 98%.

throughput, FreeFlow is sometimes worse than host TCP and cannot achive full throughput like raw RDMA, due to the overhead of socket-to-verbs translation. (but, still 6.8 to 13.4 times larger than weave with large message)


Why FreeFlow's good performance
+++++++++++++++++++++++++++++++++

two reason
1. the RDMA stack and FreeFlow's architecture works only in the userspace and avoid the context switching in kernel TCP stack. (not unique advantage; customized userspace network stack can also achive this)

2. the existing TCP/IP virtual networking solution perform packet-by-packet address translation from virtual network to host network. but, FreeFlow perform message-based translation from virtual connection to physical connection -> FreeFlow always outperform weave, through rsocket introduces some socket-to-verbs translation overhead.


Real-world Application
-------------------------

show the performance of TensorFlow and Spark running on containers.
compare the application performance on FreeFlow against Host-RDMA, Host-TCP, Weave

run on our InfiniBand cluster (not test on RoCE cluster because of none GPU??).

Tensorflow
^^^^^^^^^^^^

run RDMA-enabled tensorflow (v1.3.0) on three servers in the InfiniBand cluster
modify single line of the source code of tensorflow to replace the original memory allocation function with our custom memory alloaction (4.3)

each server
- eight NVIDIA GTX 1080Ti GPUs
- one of the servers is a master node and also a perameter server
- other two servers are workers

three specific model (image recognition)
1. ResNet-50
2. Inception-v3
3. AlexNet
and use synthetic ImageNet data as trainging data

Figure15(a)
the network performance is indeed a bottleneck in the distributed training.
compareing host RDMA with host TCP, performce 1.8 to 3.0 times better in terms of training speed.
the gap between FreeFlow and Weave (on container overlay) is even wider (ex. Alexnet, FreeFlow runs 14.6 times faster than Weave).
FreeFlow performance very close to host RDMA. (sometimes even faster, this is due to measurement noise)


speech recognition
run one private speech RNN model consisting of a bi-directional encoder and a fully connected decoder layers, with a hidden layer dimensionally of 1024 and a vocabulary size of 100k. (いやーこれはちょっと参考にならんな。。)

Figure15(b), CDF of the time spent for each training step, including the GPU time and networking time. 
again, FreeFlow is very close to host RDMA. the median training time is around 8.7 times faster than Weave.

Spark
^^^^^^^^^^

run Spark(v2.1.0) on two servers.
one server ... run master container that schedules jobs on slave containers.
Both of the servers run a slave container. The RDMA extension for Spark is implemented by is closed source.
we demonstrate the basic benchmarks shipped with the spark distribution - GroupBy and SortBy

each benchmark
- run on 262144 key-value pairs with 2KB value size.

set the number of Spark mappers and reducers to 8 and each of them is a single thread

Figure 16,
similar observations as running TensorFlow
FreeFlow performance is very close to running on host RDMA, better than host TCP, and 1.8 times better than Weave.

Releated Work
---------------

RDMA virtualization for containers
+++++++++++++++++++++++++++++++++++++

on going effort from Mellanox to extend network mnamespace and cgroup in Linux kernel to accomodate RDMA for network isolation.
usess MACVLAN to split a physical interface to multiple virtual interfaces, insert one or multiple interfaces to each container, and relies on VLAN routing to deliver traffic to the correct virtual interface. -> portability issue for cloud environment

another approache, using programmable hardware to handle RDMA virtualization for containers, such as smart NICs or FPGA. -> FreeFlow is lower cost by using commodity hardware and better flexibility to customize network feature

RDMA virtualization for VM
++++++++++++++++++++++++++++

HyV is the closest solution to FreeFlow
key difference : provide control data path (HyV instead provide performance (in private cluster))
-> not suitable for cloud environment.

VMM-bypass I/O : similar design and issue as HyV.

Vmware vRDMA
-> not working on container

Conclusion
-----------

FreeFlow : virtual RDMA networking solution that provides the isolation, portability and controllability needed in containerlized clouds.

FreeFlow is transparent to applications and achives close-to bere-metal RDMA performance with acceptable overhead.

Evaluation with read-world applications and microbenchmarks show that FreeFlow can support performance comparable to bare-metal RDMA and much better than the existing TCP/IP virtual network solution.

open source the prototype of FreeFlow!
