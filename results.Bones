
****************************
Benchmarking Bones on Fri 8 Sep 08:51:18 CEST 2017 under Linux nexion 4.11.9-1-ARCH #1 SMP PREEMPT Wed Jul 5 18:23:08 CEST 2017 x86_64 GNU/Linux

Testing browse under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/browse.scm /tmp/larcenous/Bones/browse.exe
bones /tmp/larcenous/Bones/browse.scm > /tmp/larcenous/Bones/browse.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/browse.scm.s -o /tmp/larcenous/Bones/browse.scm.o
gcc /tmp/larcenous/Bones/browse.scm.o -o /tmp/larcenous/Bones/browse.exe
Running...
Running browse:2000
Elapsed time: 0.585399 seconds (1.0) for browse:2000
+!CSVLINE!+bones-unknown,browse:2000,0.585399

real	0m0.587s
user	0m0.570s
sys	0m0.017s

Testing deriv under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/deriv.scm /tmp/larcenous/Bones/deriv.exe
bones /tmp/larcenous/Bones/deriv.scm > /tmp/larcenous/Bones/deriv.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/deriv.scm.s -o /tmp/larcenous/Bones/deriv.scm.o
gcc /tmp/larcenous/Bones/deriv.scm.o -o /tmp/larcenous/Bones/deriv.exe
Running...
Running deriv:10000000
Elapsed time: 4.674842216 seconds (4.0) for deriv:10000000
+!CSVLINE!+bones-unknown,deriv:10000000,4.674842216

real	0m4.676s
user	0m4.669s
sys	0m0.007s

Testing destruc under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/destruc.scm /tmp/larcenous/Bones/destruc.exe
bones /tmp/larcenous/Bones/destruc.scm > /tmp/larcenous/Bones/destruc.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/destruc.scm.s -o /tmp/larcenous/Bones/destruc.scm.o
gcc /tmp/larcenous/Bones/destruc.scm.o -o /tmp/larcenous/Bones/destruc.exe
Running...
Running destruc:600:50:4000
Elapsed time: 8.994957256999999 seconds (9.0) for destruc:600:50:4000
+!CSVLINE!+bones-unknown,destruc:600:50:4000,8.994957256999999

real	0m8.996s
user	0m8.993s
sys	0m0.003s

Testing diviter under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/diviter.scm /tmp/larcenous/Bones/diviter.exe
bones /tmp/larcenous/Bones/diviter.scm > /tmp/larcenous/Bones/diviter.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/diviter.scm.s -o /tmp/larcenous/Bones/diviter.scm.o
gcc /tmp/larcenous/Bones/diviter.scm.o -o /tmp/larcenous/Bones/diviter.exe
Running...
Running diviter:1000:1000000
Elapsed time: 2.395617389 seconds (3.0) for diviter:1000:1000000
+!CSVLINE!+bones-unknown,diviter:1000:1000000,2.395617389

real	0m2.397s
user	0m2.393s
sys	0m0.003s

Testing divrec under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/divrec.scm /tmp/larcenous/Bones/divrec.exe
bones /tmp/larcenous/Bones/divrec.scm > /tmp/larcenous/Bones/divrec.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/divrec.scm.s -o /tmp/larcenous/Bones/divrec.scm.o
gcc /tmp/larcenous/Bones/divrec.scm.o -o /tmp/larcenous/Bones/divrec.exe
Running...
Running divrec:1000:1000000
Elapsed time: 4.804829538 seconds (5.0) for divrec:1000:1000000
+!CSVLINE!+bones-unknown,divrec:1000:1000000,4.804829538

real	0m4.806s
user	0m4.792s
sys	0m0.013s

Testing puzzle under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/puzzle.scm /tmp/larcenous/Bones/puzzle.exe
bones /tmp/larcenous/Bones/puzzle.scm > /tmp/larcenous/Bones/puzzle.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/puzzle.scm.s -o /tmp/larcenous/Bones/puzzle.scm.o
gcc /tmp/larcenous/Bones/puzzle.scm.o -o /tmp/larcenous/Bones/puzzle.exe
Running...
Running puzzle:1000
Elapsed time: 17.026148759 seconds (17.0) for puzzle:1000
+!CSVLINE!+bones-unknown,puzzle:1000,17.026148759

real	0m17.027s
user	0m17.020s
sys	0m0.007s

Testing triangl under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/triangl.scm /tmp/larcenous/Bones/triangl.exe
bones /tmp/larcenous/Bones/triangl.scm > /tmp/larcenous/Bones/triangl.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/triangl.scm.s -o /tmp/larcenous/Bones/triangl.scm.o
gcc /tmp/larcenous/Bones/triangl.scm.o -o /tmp/larcenous/Bones/triangl.exe
Running...
Running triangl:22:1:50
Elapsed time: 10.628684909 seconds (11.0) for triangl:22:1:50
+!CSVLINE!+bones-unknown,triangl:22:1:50,10.628684909

real	0m10.630s
user	0m10.626s
sys	0m0.003s

Testing tak under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/tak.scm /tmp/larcenous/Bones/tak.exe
bones /tmp/larcenous/Bones/tak.scm > /tmp/larcenous/Bones/tak.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/tak.scm.s -o /tmp/larcenous/Bones/tak.scm.o
gcc /tmp/larcenous/Bones/tak.scm.o -o /tmp/larcenous/Bones/tak.exe
Running...
Running tak:40:20:11:1
Elapsed time: 12.508303115 seconds (13.0) for tak:40:20:11:1
+!CSVLINE!+bones-unknown,tak:40:20:11:1,12.508303115

real	0m12.509s
user	0m12.502s
sys	0m0.007s

Testing takl under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/takl.scm /tmp/larcenous/Bones/takl.exe
bones /tmp/larcenous/Bones/takl.scm > /tmp/larcenous/Bones/takl.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/takl.scm.s -o /tmp/larcenous/Bones/takl.scm.o
gcc /tmp/larcenous/Bones/takl.scm.o -o /tmp/larcenous/Bones/takl.exe
Running...
Running takl:40:20:12:1
Elapsed time: 7.595779844 seconds (8.0) for takl:40:20:12:1
+!CSVLINE!+bones-unknown,takl:40:20:12:1,7.595779844

real	0m7.597s
user	0m7.597s
sys	0m0.000s

Testing ntakl under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/ntakl.scm /tmp/larcenous/Bones/ntakl.exe
bones /tmp/larcenous/Bones/ntakl.scm > /tmp/larcenous/Bones/ntakl.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/ntakl.scm.s -o /tmp/larcenous/Bones/ntakl.scm.o
gcc /tmp/larcenous/Bones/ntakl.scm.o -o /tmp/larcenous/Bones/ntakl.exe
Running...
Running ntakl:40:20:12:1
Elapsed time: 8.144011728000001 seconds (8.0) for ntakl:40:20:12:1
+!CSVLINE!+bones-unknown,ntakl:40:20:12:1,8.144011728000001

real	0m8.145s
user	0m8.141s
sys	0m0.003s

Testing cpstak under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/cpstak.scm /tmp/larcenous/Bones/cpstak.exe
bones /tmp/larcenous/Bones/cpstak.scm > /tmp/larcenous/Bones/cpstak.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/cpstak.scm.s -o /tmp/larcenous/Bones/cpstak.scm.o
gcc /tmp/larcenous/Bones/cpstak.scm.o -o /tmp/larcenous/Bones/cpstak.exe
Running...
Running cpstak:40:20:11:1
Elapsed time: 16.370167003 seconds (17.0) for cpstak:40:20:11:1
+!CSVLINE!+bones-unknown,cpstak:40:20:11:1,16.370167003

real	0m16.371s
user	0m16.364s
sys	0m0.007s

Testing ctak under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/ctak.scm /tmp/larcenous/Bones/ctak.exe
bones /tmp/larcenous/Bones/ctak.scm > /tmp/larcenous/Bones/ctak.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/ctak.scm.s -o /tmp/larcenous/Bones/ctak.scm.o
gcc /tmp/larcenous/Bones/ctak.scm.o -o /tmp/larcenous/Bones/ctak.exe
Running...
Running ctak:32:16:8:1
Elapsed time: 1.895372084 seconds (2.0) for ctak:32:16:8:1
+!CSVLINE!+bones-unknown,ctak:32:16:8:1,1.895372084

real	0m1.896s
user	0m1.893s
sys	0m0.003s

Testing fib under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/fib.scm /tmp/larcenous/Bones/fib.exe
bones /tmp/larcenous/Bones/fib.scm > /tmp/larcenous/Bones/fib.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/fib.scm.s -o /tmp/larcenous/Bones/fib.scm.o
gcc /tmp/larcenous/Bones/fib.scm.o -o /tmp/larcenous/Bones/fib.exe
Running...
Running fib:40:5
Elapsed time: 25.450774133 seconds (26.0) for fib:40:5
+!CSVLINE!+bones-unknown,fib:40:5,25.450774133

real	0m25.452s
user	0m25.444s
sys	0m0.007s

Testing fibc under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/fibc.scm /tmp/larcenous/Bones/fibc.exe
bones /tmp/larcenous/Bones/fibc.scm > /tmp/larcenous/Bones/fibc.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/fibc.scm.s -o /tmp/larcenous/Bones/fibc.scm.o
gcc /tmp/larcenous/Bones/fibc.scm.o -o /tmp/larcenous/Bones/fibc.exe
Running...
Running fibc:30:10
Elapsed time: 2.044646763 seconds (2.0) for fibc:30:10
+!CSVLINE!+bones-unknown,fibc:30:10,2.044646763

real	0m2.046s
user	0m2.036s
sys	0m0.010s

Testing fibfp under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/fibfp.scm /tmp/larcenous/Bones/fibfp.exe
bones /tmp/larcenous/Bones/fibfp.scm > /tmp/larcenous/Bones/fibfp.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/fibfp.scm.s -o /tmp/larcenous/Bones/fibfp.scm.o
gcc /tmp/larcenous/Bones/fibfp.scm.o -o /tmp/larcenous/Bones/fibfp.exe
Running...
Running fibfp:35.0:10
Elapsed time: 5.657031262 seconds (5.0) for fibfp:35.0:10
+!CSVLINE!+bones-unknown,fibfp:35.0:10,5.657031262

real	0m5.658s
user	0m5.645s
sys	0m0.013s

Testing sum under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/sum.scm /tmp/larcenous/Bones/sum.exe
bones /tmp/larcenous/Bones/sum.scm > /tmp/larcenous/Bones/sum.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/sum.scm.s -o /tmp/larcenous/Bones/sum.scm.o
gcc /tmp/larcenous/Bones/sum.scm.o -o /tmp/larcenous/Bones/sum.exe
Running...
Running sum:10000:200000
Elapsed time: 36.828231002 seconds (36.0) for sum:10000:200000
+!CSVLINE!+bones-unknown,sum:10000:200000,36.828231002

real	0m36.829s
user	0m36.825s
sys	0m0.003s

Testing sumfp under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/sumfp.scm /tmp/larcenous/Bones/sumfp.exe
bones /tmp/larcenous/Bones/sumfp.scm > /tmp/larcenous/Bones/sumfp.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/sumfp.scm.s -o /tmp/larcenous/Bones/sumfp.scm.o
gcc /tmp/larcenous/Bones/sumfp.scm.o -o /tmp/larcenous/Bones/sumfp.exe
Running...
Running sumfp:1000000.0:500
Elapsed time: 10.670726203 seconds (11.0) for sumfp:1000000.0:500
+!CSVLINE!+bones-unknown,sumfp:1000000.0:500,10.670726203

real	0m10.677s
user	0m10.653s
sys	0m0.020s

Testing fft under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/fft.scm /tmp/larcenous/Bones/fft.exe
bones /tmp/larcenous/Bones/fft.scm > /tmp/larcenous/Bones/fft.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/fft.scm.s -o /tmp/larcenous/Bones/fft.scm.o
gcc /tmp/larcenous/Bones/fft.scm.o -o /tmp/larcenous/Bones/fft.exe
Running...
Running fft:65536:100
Elapsed time: 13.942488258 seconds (14.0) for fft:65536:100
+!CSVLINE!+bones-unknown,fft:65536:100,13.942488258

real	0m13.944s
user	0m13.943s
sys	0m0.000s

Testing mbrot under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/mbrot.scm /tmp/larcenous/Bones/mbrot.exe
bones /tmp/larcenous/Bones/mbrot.scm > /tmp/larcenous/Bones/mbrot.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/mbrot.scm.s -o /tmp/larcenous/Bones/mbrot.scm.o
gcc /tmp/larcenous/Bones/mbrot.scm.o -o /tmp/larcenous/Bones/mbrot.exe
Running...
Running mbrot:75:1000
Elapsed time: 18.931376785 seconds (19.0) for mbrot:75:1000
+!CSVLINE!+bones-unknown,mbrot:75:1000,18.931376785

real	0m18.933s
user	0m18.925s
sys	0m0.007s

Testing mbrotZ under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/mbrotZ.scm /tmp/larcenous/Bones/mbrotZ.exe
bones /tmp/larcenous/Bones/mbrotZ.scm > /tmp/larcenous/Bones/mbrotZ.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/mbrotZ.scm.s -o /tmp/larcenous/Bones/mbrotZ.scm.o
/tmp/larcenous/Bones/mbrotZ.scm.s:2336: error: symbol `____2d1_2e0_2d0_2e5i' undefined
/tmp/larcenous/Bones/mbrotZ.scm.s:2855: error: symbol `___make_2drectangular' undefined
/tmp/larcenous/Bones/mbrotZ.scm.s:3092: error: symbol `___real_2dpart' undefined
/tmp/larcenous/Bones/mbrotZ.scm.s:3122: error: symbol `___imag_2dpart' undefined
gcc /tmp/larcenous/Bones/mbrotZ.scm.o -o /tmp/larcenous/Bones/mbrotZ.exe
gcc: error: /tmp/larcenous/Bones/mbrotZ.scm.o: No such file or directory
gcc: fatal error: no input files
compilation terminated.
+!CSVLINE!+bones,mbrotZ,COMPILEERROR

Testing nucleic under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/nucleic.scm /tmp/larcenous/Bones/nucleic.exe
bones /tmp/larcenous/Bones/nucleic.scm > /tmp/larcenous/Bones/nucleic.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/nucleic.scm.s -o /tmp/larcenous/Bones/nucleic.scm.o
gcc /tmp/larcenous/Bones/nucleic.scm.o -o /tmp/larcenous/Bones/nucleic.exe
Running...
Running nucleic:50
Elapsed time: 7.381952626 seconds (7.0) for nucleic:50
+!CSVLINE!+bones-unknown,nucleic:50,7.381952626

real	0m7.383s
user	0m7.373s
sys	0m0.010s

Testing pi under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/pi.scm /tmp/larcenous/Bones/pi.exe
bones /tmp/larcenous/Bones/pi.scm > /tmp/larcenous/Bones/pi.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/pi.scm.s -o /tmp/larcenous/Bones/pi.scm.o
/tmp/larcenous/Bones/pi.scm.s:5636: error: symbol `___exact_2dinteger_2dsqrt' undefined
gcc /tmp/larcenous/Bones/pi.scm.o -o /tmp/larcenous/Bones/pi.exe
gcc: error: /tmp/larcenous/Bones/pi.scm.o: No such file or directory
gcc: fatal error: no input files
compilation terminated.
+!CSVLINE!+bones,pi,COMPILEERROR

Testing pnpoly under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/pnpoly.scm /tmp/larcenous/Bones/pnpoly.exe
bones /tmp/larcenous/Bones/pnpoly.scm > /tmp/larcenous/Bones/pnpoly.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/pnpoly.scm.s -o /tmp/larcenous/Bones/pnpoly.scm.o
gcc /tmp/larcenous/Bones/pnpoly.scm.o -o /tmp/larcenous/Bones/pnpoly.exe
Running...
Running pnpoly:1000000
Elapsed time: 14.158814191 seconds (14.0) for pnpoly:1000000
+!CSVLINE!+bones-unknown,pnpoly:1000000,14.158814191

real	0m14.201s
user	0m14.156s
sys	0m0.003s

Testing ray under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/ray.scm /tmp/larcenous/Bones/ray.exe
bones /tmp/larcenous/Bones/ray.scm > /tmp/larcenous/Bones/ray.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/ray.scm.s -o /tmp/larcenous/Bones/ray.scm.o
/tmp/larcenous/Bones/ray.scm.s:23550: error: invalid character in floating-point constant exponent: '.'
gcc /tmp/larcenous/Bones/ray.scm.o -o /tmp/larcenous/Bones/ray.exe
gcc: error: /tmp/larcenous/Bones/ray.scm.o: No such file or directory
gcc: fatal error: no input files
compilation terminated.
+!CSVLINE!+bones,ray,COMPILEERROR

Testing simplex under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/simplex.scm /tmp/larcenous/Bones/simplex.exe
bones /tmp/larcenous/Bones/simplex.scm > /tmp/larcenous/Bones/simplex.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/simplex.scm.s -o /tmp/larcenous/Bones/simplex.scm.o
gcc /tmp/larcenous/Bones/simplex.scm.o -o /tmp/larcenous/Bones/simplex.exe
Running...
Running simplex:1000000
Elapsed time: 12.072283852 seconds (12.0) for simplex:1000000
+!CSVLINE!+bones-unknown,simplex:1000000,12.072283852

real	0m12.075s
user	0m12.073s
sys	0m0.000s

Testing ack under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/ack.scm /tmp/larcenous/Bones/ack.exe
bones /tmp/larcenous/Bones/ack.scm > /tmp/larcenous/Bones/ack.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/ack.scm.s -o /tmp/larcenous/Bones/ack.scm.o
gcc /tmp/larcenous/Bones/ack.scm.o -o /tmp/larcenous/Bones/ack.exe
Running...
Running ack:3:12:2
Elapsed time: 27.177224655 seconds (27.0) for ack:3:12:2
+!CSVLINE!+bones-unknown,ack:3:12:2,27.177224655

real	0m27.180s
user	0m27.177s
sys	0m0.000s

Testing array1 under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/array1.scm /tmp/larcenous/Bones/array1.exe
bones /tmp/larcenous/Bones/array1.scm > /tmp/larcenous/Bones/array1.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/array1.scm.s -o /tmp/larcenous/Bones/array1.scm.o
gcc /tmp/larcenous/Bones/array1.scm.o -o /tmp/larcenous/Bones/array1.exe
Running...
Running array1:1000000:500
Elapsed time: 21.292328549 seconds (21.0) for array1:1000000:500
+!CSVLINE!+bones-unknown,array1:1000000:500,21.292328549

real	0m21.293s
user	0m21.289s
sys	0m0.003s

Testing string under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/string.scm /tmp/larcenous/Bones/string.exe
bones /tmp/larcenous/Bones/string.scm > /tmp/larcenous/Bones/string.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/string.scm.s -o /tmp/larcenous/Bones/string.scm.o
gcc /tmp/larcenous/Bones/string.scm.o -o /tmp/larcenous/Bones/string.exe
Running...
Running string:500000:25
Elapsed time: 0.065478051 seconds (0.0) for string:500000:25
+!CSVLINE!+bones-unknown,string:500000:25,0.065478051

real	0m0.068s
user	0m0.059s
sys	0m0.007s

Testing sum1 under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/sum1.scm /tmp/larcenous/Bones/sum1.exe
bones /tmp/larcenous/Bones/sum1.scm > /tmp/larcenous/Bones/sum1.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/sum1.scm.s -o /tmp/larcenous/Bones/sum1.scm.o
gcc /tmp/larcenous/Bones/sum1.scm.o -o /tmp/larcenous/Bones/sum1.exe
Running...
Running sum1:25
Elapsed time: 11.582401452 seconds (11.0) for sum1:25
+!CSVLINE!+bones-unknown,sum1:25,11.582401452

real	0m11.585s
user	0m7.731s
sys	0m3.852s

Testing cat under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/cat.scm /tmp/larcenous/Bones/cat.exe
bones /tmp/larcenous/Bones/cat.scm > /tmp/larcenous/Bones/cat.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/cat.scm.s -o /tmp/larcenous/Bones/cat.scm.o
gcc /tmp/larcenous/Bones/cat.scm.o -o /tmp/larcenous/Bones/cat.exe
Running...
Running cat:50
Elapsed time: 175.822888339 seconds (176.0) for cat:50
+!CSVLINE!+bones-unknown,cat:50,175.822888339

real	2m55.825s
user	0m15.179s
sys	2m40.627s

Testing tail under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/tail.scm /tmp/larcenous/Bones/tail.exe
bones /tmp/larcenous/Bones/tail.scm > /tmp/larcenous/Bones/tail.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/tail.scm.s -o /tmp/larcenous/Bones/tail.scm.o
/tmp/larcenous/Bones/tail.scm.s:2502: error: symbol `___read_2dline' undefined
gcc /tmp/larcenous/Bones/tail.scm.o -o /tmp/larcenous/Bones/tail.exe
gcc: error: /tmp/larcenous/Bones/tail.scm.o: No such file or directory
gcc: fatal error: no input files
compilation terminated.
+!CSVLINE!+bones,tail,COMPILEERROR

Testing wc under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/wc.scm /tmp/larcenous/Bones/wc.exe
bones /tmp/larcenous/Bones/wc.scm > /tmp/larcenous/Bones/wc.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/wc.scm.s -o /tmp/larcenous/Bones/wc.scm.o
gcc /tmp/larcenous/Bones/wc.scm.o -o /tmp/larcenous/Bones/wc.exe
Running...
Running wc:inputs/bib:50
Elapsed time: 43.212993672 seconds (43.0) for wc:inputs/bib:50
+!CSVLINE!+bones-unknown,wc:inputs/bib:50,43.212993672

real	0m43.214s
user	0m11.473s
sys	0m31.740s

Testing read1 under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/read1.scm /tmp/larcenous/Bones/read1.exe
bones /tmp/larcenous/Bones/read1.scm > /tmp/larcenous/Bones/read1.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/read1.scm.s -o /tmp/larcenous/Bones/read1.scm.o
gcc /tmp/larcenous/Bones/read1.scm.o -o /tmp/larcenous/Bones/read1.exe
Running...
Running read1:2500
Elapsed time: 19.685648568 seconds (20.0) for read1:2500
+!CSVLINE!+bones-unknown,read1:2500,19.685648568

real	0m19.699s
user	0m9.224s
sys	0m10.457s

Testing compiler under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/compiler.scm /tmp/larcenous/Bones/compiler.exe
bones /tmp/larcenous/Bones/compiler.scm > /tmp/larcenous/Bones/compiler.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/compiler.scm.s -o /tmp/larcenous/Bones/compiler.scm.o
/tmp/larcenous/Bones/compiler.scm.s:76316: error: symbol `___real_2dpart' undefined
/tmp/larcenous/Bones/compiler.scm.s:76350: error: symbol `___imag_2dpart' undefined
/tmp/larcenous/Bones/compiler.scm.s:76458: error: symbol `___numerator' undefined
/tmp/larcenous/Bones/compiler.scm.s:76492: error: symbol `___denominator' undefined
/tmp/larcenous/Bones/compiler.scm.s:107201: error: symbol `___real_2dpart' undefined
/tmp/larcenous/Bones/compiler.scm.s:107250: error: symbol `___imag_2dpart' undefined
/tmp/larcenous/Bones/compiler.scm.s:107296: error: symbol `___imag_2dpart' undefined
gcc /tmp/larcenous/Bones/compiler.scm.o -o /tmp/larcenous/Bones/compiler.exe
gcc: error: /tmp/larcenous/Bones/compiler.scm.o: No such file or directory
gcc: fatal error: no input files
compilation terminated.
+!CSVLINE!+bones,compiler,COMPILEERROR

Testing conform under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/conform.scm /tmp/larcenous/Bones/conform.exe
bones /tmp/larcenous/Bones/conform.scm > /tmp/larcenous/Bones/conform.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/conform.scm.s -o /tmp/larcenous/Bones/conform.scm.o
gcc /tmp/larcenous/Bones/conform.scm.o -o /tmp/larcenous/Bones/conform.exe
Running...
Running conform:500
Elapsed time: 8.174557391 seconds (8.0) for conform:500
+!CSVLINE!+bones-unknown,conform:500,8.174557391

real	0m8.176s
user	0m8.169s
sys	0m0.007s

Testing dynamic under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/dynamic.scm /tmp/larcenous/Bones/dynamic.exe
bones /tmp/larcenous/Bones/dynamic.scm > /tmp/larcenous/Bones/dynamic.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/dynamic.scm.s -o /tmp/larcenous/Bones/dynamic.scm.o
gcc /tmp/larcenous/Bones/dynamic.scm.o -o /tmp/larcenous/Bones/dynamic.exe
Running...
Running dynamic:500
Elapsed time: 22.693649955 seconds (22.0) for dynamic:500
+!CSVLINE!+bones-unknown,dynamic:500,22.693649955

real	0m22.695s
user	0m16.878s
sys	0m5.816s

Testing earley under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/earley.scm /tmp/larcenous/Bones/earley.exe
bones /tmp/larcenous/Bones/earley.scm > /tmp/larcenous/Bones/earley.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/earley.scm.s -o /tmp/larcenous/Bones/earley.scm.o
gcc /tmp/larcenous/Bones/earley.scm.o -o /tmp/larcenous/Bones/earley.exe
Running...
Running earley:1

Error: out of memory

real	0m6.923s
user	0m6.916s
sys	0m0.007s
+!CSVLINE!+bones,earley,CRASHED

Testing graphs under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/graphs.scm /tmp/larcenous/Bones/graphs.exe
bones /tmp/larcenous/Bones/graphs.scm > /tmp/larcenous/Bones/graphs.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/graphs.scm.s -o /tmp/larcenous/Bones/graphs.scm.o
gcc /tmp/larcenous/Bones/graphs.scm.o -o /tmp/larcenous/Bones/graphs.exe
Running...
Running graphs:7:3
Elapsed time: 22.64577157 seconds (23.0) for graphs:7:3
+!CSVLINE!+bones-unknown,graphs:7:3,22.64577157

real	0m22.647s
user	0m22.643s
sys	0m0.003s

Testing lattice under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/lattice.scm /tmp/larcenous/Bones/lattice.exe
bones /tmp/larcenous/Bones/lattice.scm > /tmp/larcenous/Bones/lattice.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/lattice.scm.s -o /tmp/larcenous/Bones/lattice.scm.o
gcc /tmp/larcenous/Bones/lattice.scm.o -o /tmp/larcenous/Bones/lattice.exe
Running...
Running lattice:44:10
Elapsed time: 24.47331277 seconds (24.0) for lattice:44:10
+!CSVLINE!+bones-unknown,lattice:44:10,24.47331277

real	0m24.476s
user	0m24.464s
sys	0m0.010s

Testing matrix under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/matrix.scm /tmp/larcenous/Bones/matrix.exe
bones /tmp/larcenous/Bones/matrix.scm > /tmp/larcenous/Bones/matrix.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/matrix.scm.s -o /tmp/larcenous/Bones/matrix.scm.o
gcc /tmp/larcenous/Bones/matrix.scm.o -o /tmp/larcenous/Bones/matrix.exe
Running...
Running matrix:5:5:2500
Elapsed time: 6.36992203 seconds (6.0) for matrix:5:5:2500
+!CSVLINE!+bones-unknown,matrix:5:5:2500,6.36992203

real	0m6.373s
user	0m6.371s
sys	0m0.000s

Testing maze under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/maze.scm /tmp/larcenous/Bones/maze.exe
bones /tmp/larcenous/Bones/maze.scm > /tmp/larcenous/Bones/maze.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/maze.scm.s -o /tmp/larcenous/Bones/maze.scm.o
gcc /tmp/larcenous/Bones/maze.scm.o -o /tmp/larcenous/Bones/maze.exe
Running...
Running maze:20:7:10000
Elapsed time: 4.681313925 seconds (5.0) for maze:20:7:10000
+!CSVLINE!+bones-unknown,maze:20:7:10000,4.681313925

real	0m4.685s
user	0m4.677s
sys	0m0.007s

Testing mazefun under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/mazefun.scm /tmp/larcenous/Bones/mazefun.exe
bones /tmp/larcenous/Bones/mazefun.scm > /tmp/larcenous/Bones/mazefun.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/mazefun.scm.s -o /tmp/larcenous/Bones/mazefun.scm.o
gcc /tmp/larcenous/Bones/mazefun.scm.o -o /tmp/larcenous/Bones/mazefun.exe
Running...
Running mazefun:11:11:10000
Elapsed time: 10.592938363 seconds (11.0) for mazefun:11:11:10000
+!CSVLINE!+bones-unknown,mazefun:11:11:10000,10.592938363

real	0m10.594s
user	0m10.591s
sys	0m0.003s

Testing nqueens under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/nqueens.scm /tmp/larcenous/Bones/nqueens.exe
bones /tmp/larcenous/Bones/nqueens.scm > /tmp/larcenous/Bones/nqueens.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/nqueens.scm.s -o /tmp/larcenous/Bones/nqueens.scm.o
gcc /tmp/larcenous/Bones/nqueens.scm.o -o /tmp/larcenous/Bones/nqueens.exe
Running...
Running nqueens:13:10
Elapsed time: 33.09309765 seconds (34.0) for nqueens:13:10
+!CSVLINE!+bones-unknown,nqueens:13:10,33.09309765

real	0m33.096s
user	0m33.087s
sys	0m0.007s

Testing paraffins under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/paraffins.scm /tmp/larcenous/Bones/paraffins.exe
bones /tmp/larcenous/Bones/paraffins.scm > /tmp/larcenous/Bones/paraffins.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/paraffins.scm.s -o /tmp/larcenous/Bones/paraffins.scm.o
gcc /tmp/larcenous/Bones/paraffins.scm.o -o /tmp/larcenous/Bones/paraffins.exe
Running...
Running paraffins:23:10

Error: out of memory

real	0m0.948s
user	0m0.947s
sys	0m0.000s
+!CSVLINE!+bones,paraffins,CRASHED

Testing parsing under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/parsing.scm /tmp/larcenous/Bones/parsing.exe
bones /tmp/larcenous/Bones/parsing.scm > /tmp/larcenous/Bones/parsing.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/parsing.scm.s -o /tmp/larcenous/Bones/parsing.scm.o
gcc /tmp/larcenous/Bones/parsing.scm.o -o /tmp/larcenous/Bones/parsing.exe
Running...
Running parsing:2500
Elapsed time: 23.706750772 seconds (24.0) for parsing:2500
+!CSVLINE!+bones-unknown,parsing:2500,23.706750772

real	0m23.709s
user	0m23.690s
sys	0m0.017s

Testing peval under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/peval.scm /tmp/larcenous/Bones/peval.exe
bones /tmp/larcenous/Bones/peval.scm > /tmp/larcenous/Bones/peval.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/peval.scm.s -o /tmp/larcenous/Bones/peval.scm.o
gcc /tmp/larcenous/Bones/peval.scm.o -o /tmp/larcenous/Bones/peval.exe
Running...
Running peval:2000
Elapsed time: 4.920942133 seconds (5.0) for peval:2000
+!CSVLINE!+bones-unknown,peval:2000,4.920942133

real	0m4.924s
user	0m4.919s
sys	0m0.003s

Testing primes under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/primes.scm /tmp/larcenous/Bones/primes.exe
bones /tmp/larcenous/Bones/primes.scm > /tmp/larcenous/Bones/primes.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/primes.scm.s -o /tmp/larcenous/Bones/primes.scm.o
gcc /tmp/larcenous/Bones/primes.scm.o -o /tmp/larcenous/Bones/primes.exe
Running...
Running primes:1000:10000
Elapsed time: 3.625404884 seconds (4.0) for primes:1000:10000
+!CSVLINE!+bones-unknown,primes:1000:10000,3.625404884

real	0m3.628s
user	0m3.620s
sys	0m0.007s

Testing quicksort under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/quicksort.scm /tmp/larcenous/Bones/quicksort.exe
bones /tmp/larcenous/Bones/quicksort.scm > /tmp/larcenous/Bones/quicksort.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/quicksort.scm.s -o /tmp/larcenous/Bones/quicksort.scm.o
/tmp/larcenous/Bones/quicksort.scm.s:2743: error: symbol `___vector_2dmap' undefined
gcc /tmp/larcenous/Bones/quicksort.scm.o -o /tmp/larcenous/Bones/quicksort.exe
gcc: error: /tmp/larcenous/Bones/quicksort.scm.o: No such file or directory
gcc: fatal error: no input files
compilation terminated.
+!CSVLINE!+bones,quicksort,COMPILEERROR

Testing scheme under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/scheme.scm /tmp/larcenous/Bones/scheme.exe
bones /tmp/larcenous/Bones/scheme.scm > /tmp/larcenous/Bones/scheme.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/scheme.scm.s -o /tmp/larcenous/Bones/scheme.scm.o
/tmp/larcenous/Bones/scheme.scm.s:3607: error: symbol `___complex_3f' undefined
gcc /tmp/larcenous/Bones/scheme.scm.o -o /tmp/larcenous/Bones/scheme.exe
gcc: error: /tmp/larcenous/Bones/scheme.scm.o: No such file or directory
gcc: fatal error: no input files
compilation terminated.
+!CSVLINE!+bones,scheme,COMPILEERROR

Testing slatex under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/slatex.scm /tmp/larcenous/Bones/slatex.exe
bones /tmp/larcenous/Bones/slatex.scm > /tmp/larcenous/Bones/slatex.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/slatex.scm.s -o /tmp/larcenous/Bones/slatex.scm.o
gcc /tmp/larcenous/Bones/slatex.scm.o -o /tmp/larcenous/Bones/slatex.exe
Running...
Running slatex:500
Elapsed time: 8.336757885000001 seconds (9.0) for slatex:500
+!CSVLINE!+bones-unknown,slatex:500,8.336757885000001

real	0m8.339s
user	0m2.244s
sys	0m6.069s

Testing chudnovsky under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/chudnovsky.scm /tmp/larcenous/Bones/chudnovsky.exe
bones /tmp/larcenous/Bones/chudnovsky.scm > /tmp/larcenous/Bones/chudnovsky.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/chudnovsky.scm.s -o /tmp/larcenous/Bones/chudnovsky.scm.o
/tmp/larcenous/Bones/chudnovsky.scm.s:3539: error: symbol `___exact_2dinteger_2dsqrt' undefined
gcc /tmp/larcenous/Bones/chudnovsky.scm.o -o /tmp/larcenous/Bones/chudnovsky.exe
gcc: error: /tmp/larcenous/Bones/chudnovsky.scm.o: No such file or directory
gcc: fatal error: no input files
compilation terminated.
+!CSVLINE!+bones,chudnovsky,COMPILEERROR

Testing nboyer under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/nboyer.scm /tmp/larcenous/Bones/nboyer.exe
bones /tmp/larcenous/Bones/nboyer.scm > /tmp/larcenous/Bones/nboyer.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/nboyer.scm.s -o /tmp/larcenous/Bones/nboyer.scm.o
gcc /tmp/larcenous/Bones/nboyer.scm.o -o /tmp/larcenous/Bones/nboyer.exe
Running...
Running nboyer:5:1

Error: out of memory

real	0m4.592s
user	0m4.584s
sys	0m0.007s
+!CSVLINE!+bones,nboyer,CRASHED

Testing sboyer under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/sboyer.scm /tmp/larcenous/Bones/sboyer.exe
bones /tmp/larcenous/Bones/sboyer.scm > /tmp/larcenous/Bones/sboyer.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/sboyer.scm.s -o /tmp/larcenous/Bones/sboyer.scm.o
gcc /tmp/larcenous/Bones/sboyer.scm.o -o /tmp/larcenous/Bones/sboyer.exe
Running...
Running sboyer:5:1

Error: out of memory

real	0m30.405s
user	0m30.389s
sys	0m0.013s
+!CSVLINE!+bones,sboyer,CRASHED

Testing gcbench under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/gcbench.scm /tmp/larcenous/Bones/gcbench.exe
bones /tmp/larcenous/Bones/gcbench.scm > /tmp/larcenous/Bones/gcbench.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/gcbench.scm.s -o /tmp/larcenous/Bones/gcbench.scm.o
/tmp/larcenous/Bones/gcbench.scm.s:2877: error: symbol `___make_2dnode_2draw' undefined
/tmp/larcenous/Bones/gcbench.scm.s:2878: error: symbol `___left' undefined
/tmp/larcenous/Bones/gcbench.scm.s:2879: error: symbol `___right' undefined
/tmp/larcenous/Bones/gcbench.scm.s:2880: error: symbol `___i' undefined
/tmp/larcenous/Bones/gcbench.scm.s:2881: error: symbol `___j' undefined
/tmp/larcenous/Bones/gcbench.scm.s:2909: error: symbol `___left' undefined
/tmp/larcenous/Bones/gcbench.scm.s:2910: error: symbol `___node_2eleft' undefined
/tmp/larcenous/Bones/gcbench.scm.s:2911: error: symbol `___node_2eleft_2dset_21' undefined
/tmp/larcenous/Bones/gcbench.scm.s:2943: error: symbol `___right' undefined
/tmp/larcenous/Bones/gcbench.scm.s:2944: error: symbol `___node_2eright' undefined
/tmp/larcenous/Bones/gcbench.scm.s:2945: error: symbol `___node_2eright_2dset_21' undefined
/tmp/larcenous/Bones/gcbench.scm.s:2979: error: symbol `___i' undefined
/tmp/larcenous/Bones/gcbench.scm.s:2980: error: symbol `___node_2ei' undefined
/tmp/larcenous/Bones/gcbench.scm.s:2981: error: symbol `___node_2ei_2dset_21' undefined
/tmp/larcenous/Bones/gcbench.scm.s:3017: error: symbol `___j' undefined
/tmp/larcenous/Bones/gcbench.scm.s:3018: error: symbol `___node_2ej' undefined
/tmp/larcenous/Bones/gcbench.scm.s:3019: error: symbol `___node_2ej_2dset_21' undefined
/tmp/larcenous/Bones/gcbench.scm.s:3029: error: symbol `___class_4eode' undefined
/tmp/larcenous/Bones/gcbench.scm.s:3031: error: symbol `___class_4eode_3f' undefined
/tmp/larcenous/Bones/gcbench.scm.s:3053: error: symbol `___define_2drecord_2dtype' undefined
/tmp/larcenous/Bones/gcbench.scm.s:5525: error: symbol `___node_2eleft_2dset_21' undefined
/tmp/larcenous/Bones/gcbench.scm.s:5575: error: symbol `___node_2eright_2dset_21' undefined
/tmp/larcenous/Bones/gcbench.scm.s:5597: error: symbol `___node_2eleft' undefined
/tmp/larcenous/Bones/gcbench.scm.s:5650: error: symbol `___node_2eright' undefined
/tmp/larcenous/Bones/gcbench.scm.s:5680: error: symbol `___make_2dnode_2draw' undefined
/tmp/larcenous/Bones/gcbench.scm.s:5694: error: symbol `___make_2dnode_2draw' undefined
gcc /tmp/larcenous/Bones/gcbench.scm.o -o /tmp/larcenous/Bones/gcbench.exe
gcc: error: /tmp/larcenous/Bones/gcbench.scm.o: No such file or directory
gcc: fatal error: no input files
compilation terminated.
+!CSVLINE!+bones,gcbench,COMPILEERROR

Testing mperm under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/mperm.scm /tmp/larcenous/Bones/mperm.exe
bones /tmp/larcenous/Bones/mperm.scm > /tmp/larcenous/Bones/mperm.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/mperm.scm.s -o /tmp/larcenous/Bones/mperm.scm.o
gcc /tmp/larcenous/Bones/mperm.scm.o -o /tmp/larcenous/Bones/mperm.exe
Running...

Error: out of memory

real	0m2.221s
user	0m2.218s
sys	0m0.003s
+!CSVLINE!+bones,mperm,CRASHED

Testing equal under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/equal.scm /tmp/larcenous/Bones/equal.exe
bones /tmp/larcenous/Bones/equal.scm > /tmp/larcenous/Bones/equal.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/equal.scm.s -o /tmp/larcenous/Bones/equal.scm.o
gcc /tmp/larcenous/Bones/equal.scm.o -o /tmp/larcenous/Bones/equal.exe
Running...
Running equal:100:100:8:1000:2000:5000
./bench: line 540: 31054 Killed                  "$1" < "$2"

real	5m0.003s
user	4m59.996s
sys	0m0.000s
+!CSVLINE!+bones,equal,ULIMITKILLED

Testing bv2string under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/bv2string.scm /tmp/larcenous/Bones/bv2string.exe
bones /tmp/larcenous/Bones/bv2string.scm > /tmp/larcenous/Bones/bv2string.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/bv2string.scm.s -o /tmp/larcenous/Bones/bv2string.scm.o
/tmp/larcenous/Bones/bv2string.scm.s:2864: error: symbol `___utf8_2d_3estring' undefined
/tmp/larcenous/Bones/bv2string.scm.s:2865: error: symbol `___string_2d_3eutf8' undefined
/tmp/larcenous/Bones/bv2string.scm.s:3008: error: symbol `___utf8_2d_3estring' undefined
/tmp/larcenous/Bones/bv2string.scm.s:3009: error: symbol `___string_2d_3eutf8' undefined
gcc /tmp/larcenous/Bones/bv2string.scm.o -o /tmp/larcenous/Bones/bv2string.exe
gcc: error: /tmp/larcenous/Bones/bv2string.scm.o: No such file or directory
gcc: fatal error: no input files
compilation terminated.
+!CSVLINE!+bones,bv2string,COMPILEERROR
