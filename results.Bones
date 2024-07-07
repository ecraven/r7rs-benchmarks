
****************************
Benchmarking Bones on Fri 5 Jul 12:04:49 CEST 2024 under Linux h4nex 6.9.7-arch1-1 #1 SMP PREEMPT_DYNAMIC Fri, 28 Jun 2024 04:32:50 +0000 x86_64 GNU/Linux

Testing browse under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/browse.scm /tmp/r7rs-benchmarks/Bones/browse.exe
bones /tmp/r7rs-benchmarks/Bones/browse.scm > /tmp/r7rs-benchmarks/Bones/browse.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/browse.scm.s -o /tmp/r7rs-benchmarks/Bones/browse.scm.o
gcc /tmp/r7rs-benchmarks/Bones/browse.scm.o -o /tmp/r7rs-benchmarks/Bones/browse.exe
Running...
Running browse:2000
Elapsed time: 0.465687686 seconds (1.0) for browse:2000
+!CSVLINE!+bones-8,browse:2000,0.465687686

real	0m0.468s
user	0m0.457s
sys	0m0.010s

Testing deriv under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/deriv.scm /tmp/r7rs-benchmarks/Bones/deriv.exe
bones /tmp/r7rs-benchmarks/Bones/deriv.scm > /tmp/r7rs-benchmarks/Bones/deriv.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/deriv.scm.s -o /tmp/r7rs-benchmarks/Bones/deriv.scm.o
gcc /tmp/r7rs-benchmarks/Bones/deriv.scm.o -o /tmp/r7rs-benchmarks/Bones/deriv.exe
Running...
Running deriv:10000000
Elapsed time: 3.622516904 seconds (3.0) for deriv:10000000
+!CSVLINE!+bones-8,deriv:10000000,3.622516904

real	0m3.624s
user	0m3.598s
sys	0m0.013s

Testing destruc under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/destruc.scm /tmp/r7rs-benchmarks/Bones/destruc.exe
bones /tmp/r7rs-benchmarks/Bones/destruc.scm > /tmp/r7rs-benchmarks/Bones/destruc.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/destruc.scm.s -o /tmp/r7rs-benchmarks/Bones/destruc.scm.o
gcc /tmp/r7rs-benchmarks/Bones/destruc.scm.o -o /tmp/r7rs-benchmarks/Bones/destruc.exe
Running...
Running destruc:600:50:4000
Elapsed time: 7.069072658 seconds (8.0) for destruc:600:50:4000
+!CSVLINE!+bones-8,destruc:600:50:4000,7.069072658

real	0m7.071s
user	0m7.036s
sys	0m0.013s

Testing diviter under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/diviter.scm /tmp/r7rs-benchmarks/Bones/diviter.exe
bones /tmp/r7rs-benchmarks/Bones/diviter.scm > /tmp/r7rs-benchmarks/Bones/diviter.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/diviter.scm.s -o /tmp/r7rs-benchmarks/Bones/diviter.scm.o
gcc /tmp/r7rs-benchmarks/Bones/diviter.scm.o -o /tmp/r7rs-benchmarks/Bones/diviter.exe
Running...
Running diviter:1000:1000000
Elapsed time: 1.639367853 seconds (2.0) for diviter:1000:1000000
+!CSVLINE!+bones-8,diviter:1000:1000000,1.639367853

real	0m1.641s
user	0m1.633s
sys	0m0.003s

Testing divrec under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/divrec.scm /tmp/r7rs-benchmarks/Bones/divrec.exe
bones /tmp/r7rs-benchmarks/Bones/divrec.scm > /tmp/r7rs-benchmarks/Bones/divrec.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/divrec.scm.s -o /tmp/r7rs-benchmarks/Bones/divrec.scm.o
gcc /tmp/r7rs-benchmarks/Bones/divrec.scm.o -o /tmp/r7rs-benchmarks/Bones/divrec.exe
Running...
Running divrec:1000:1000000
Elapsed time: 3.430187832 seconds (3.0) for divrec:1000:1000000
+!CSVLINE!+bones-8,divrec:1000:1000000,3.430187832

real	0m3.431s
user	0m3.409s
sys	0m0.013s

Testing puzzle under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/puzzle.scm /tmp/r7rs-benchmarks/Bones/puzzle.exe
bones /tmp/r7rs-benchmarks/Bones/puzzle.scm > /tmp/r7rs-benchmarks/Bones/puzzle.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/puzzle.scm.s -o /tmp/r7rs-benchmarks/Bones/puzzle.scm.o
gcc /tmp/r7rs-benchmarks/Bones/puzzle.scm.o -o /tmp/r7rs-benchmarks/Bones/puzzle.exe
Running...
Running puzzle:1000
Elapsed time: 12.458260697 seconds (13.0) for puzzle:1000
+!CSVLINE!+bones-8,puzzle:1000,12.458260697

real	0m12.459s
user	0m12.419s
sys	0m0.007s

Testing triangl under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/triangl.scm /tmp/r7rs-benchmarks/Bones/triangl.exe
bones /tmp/r7rs-benchmarks/Bones/triangl.scm > /tmp/r7rs-benchmarks/Bones/triangl.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/triangl.scm.s -o /tmp/r7rs-benchmarks/Bones/triangl.scm.o
gcc /tmp/r7rs-benchmarks/Bones/triangl.scm.o -o /tmp/r7rs-benchmarks/Bones/triangl.exe
Running...
Running triangl:22:1:50
Elapsed time: 9.635094047999999 seconds (9.0) for triangl:22:1:50
+!CSVLINE!+bones-8,triangl:22:1:50,9.635094047999999

real	0m9.636s
user	0m9.596s
sys	0m0.013s

Testing tak under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/tak.scm /tmp/r7rs-benchmarks/Bones/tak.exe
bones /tmp/r7rs-benchmarks/Bones/tak.scm > /tmp/r7rs-benchmarks/Bones/tak.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/tak.scm.s -o /tmp/r7rs-benchmarks/Bones/tak.scm.o
gcc /tmp/r7rs-benchmarks/Bones/tak.scm.o -o /tmp/r7rs-benchmarks/Bones/tak.exe
Running...
Running tak:40:20:11:1
Elapsed time: 10.839629231 seconds (11.0) for tak:40:20:11:1
+!CSVLINE!+bones-8,tak:40:20:11:1,10.839629231

real	0m10.841s
user	0m10.801s
sys	0m0.010s

Testing takl under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/takl.scm /tmp/r7rs-benchmarks/Bones/takl.exe
bones /tmp/r7rs-benchmarks/Bones/takl.scm > /tmp/r7rs-benchmarks/Bones/takl.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/takl.scm.s -o /tmp/r7rs-benchmarks/Bones/takl.scm.o
gcc /tmp/r7rs-benchmarks/Bones/takl.scm.o -o /tmp/r7rs-benchmarks/Bones/takl.exe
Running...
Running takl:40:20:12:1
Elapsed time: 6.208265091 seconds (6.0) for takl:40:20:12:1
+!CSVLINE!+bones-8,takl:40:20:12:1,6.208265091

real	0m6.210s
user	0m6.185s
sys	0m0.010s

Testing ntakl under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/ntakl.scm /tmp/r7rs-benchmarks/Bones/ntakl.exe
bones /tmp/r7rs-benchmarks/Bones/ntakl.scm > /tmp/r7rs-benchmarks/Bones/ntakl.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/ntakl.scm.s -o /tmp/r7rs-benchmarks/Bones/ntakl.scm.o
gcc /tmp/r7rs-benchmarks/Bones/ntakl.scm.o -o /tmp/r7rs-benchmarks/Bones/ntakl.exe
Running...
Running ntakl:40:20:12:1
Elapsed time: 5.928984281 seconds (6.0) for ntakl:40:20:12:1
+!CSVLINE!+bones-8,ntakl:40:20:12:1,5.928984281

real	0m5.930s
user	0m5.908s
sys	0m0.007s

Testing cpstak under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/cpstak.scm /tmp/r7rs-benchmarks/Bones/cpstak.exe
bones /tmp/r7rs-benchmarks/Bones/cpstak.scm > /tmp/r7rs-benchmarks/Bones/cpstak.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/cpstak.scm.s -o /tmp/r7rs-benchmarks/Bones/cpstak.scm.o
gcc /tmp/r7rs-benchmarks/Bones/cpstak.scm.o -o /tmp/r7rs-benchmarks/Bones/cpstak.exe
Running...
Running cpstak:40:20:11:1
Elapsed time: 13.008677585 seconds (13.0) for cpstak:40:20:11:1
+!CSVLINE!+bones-8,cpstak:40:20:11:1,13.008677585

real	0m13.010s
user	0m12.959s
sys	0m0.013s

Testing ctak under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/ctak.scm /tmp/r7rs-benchmarks/Bones/ctak.exe
bones /tmp/r7rs-benchmarks/Bones/ctak.scm > /tmp/r7rs-benchmarks/Bones/ctak.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/ctak.scm.s -o /tmp/r7rs-benchmarks/Bones/ctak.scm.o
gcc /tmp/r7rs-benchmarks/Bones/ctak.scm.o -o /tmp/r7rs-benchmarks/Bones/ctak.exe
Running...
Running ctak:32:16:8:1
Elapsed time: 1.598913726 seconds (1.0) for ctak:32:16:8:1
+!CSVLINE!+bones-8,ctak:32:16:8:1,1.598913726

real	0m1.600s
user	0m1.582s
sys	0m0.014s

Testing fib under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/fib.scm /tmp/r7rs-benchmarks/Bones/fib.exe
bones /tmp/r7rs-benchmarks/Bones/fib.scm > /tmp/r7rs-benchmarks/Bones/fib.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/fib.scm.s -o /tmp/r7rs-benchmarks/Bones/fib.scm.o
gcc /tmp/r7rs-benchmarks/Bones/fib.scm.o -o /tmp/r7rs-benchmarks/Bones/fib.exe
Running...
Running fib:40:5
Elapsed time: 24.628409718 seconds (25.0) for fib:40:5
+!CSVLINE!+bones-8,fib:40:5,24.628409718

real	0m24.630s
user	0m24.547s
sys	0m0.017s

Testing fibc under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/fibc.scm /tmp/r7rs-benchmarks/Bones/fibc.exe
bones /tmp/r7rs-benchmarks/Bones/fibc.scm > /tmp/r7rs-benchmarks/Bones/fibc.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/fibc.scm.s -o /tmp/r7rs-benchmarks/Bones/fibc.scm.o
gcc /tmp/r7rs-benchmarks/Bones/fibc.scm.o -o /tmp/r7rs-benchmarks/Bones/fibc.exe
Running...
Running fibc:30:10
Elapsed time: 1.786619176 seconds (2.0) for fibc:30:10
+!CSVLINE!+bones-8,fibc:30:10,1.786619176

real	0m1.788s
user	0m1.776s
sys	0m0.007s

Testing fibfp under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/fibfp.scm /tmp/r7rs-benchmarks/Bones/fibfp.exe
bones /tmp/r7rs-benchmarks/Bones/fibfp.scm > /tmp/r7rs-benchmarks/Bones/fibfp.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/fibfp.scm.s -o /tmp/r7rs-benchmarks/Bones/fibfp.scm.o
gcc /tmp/r7rs-benchmarks/Bones/fibfp.scm.o -o /tmp/r7rs-benchmarks/Bones/fibfp.exe
Running...
Running fibfp:35.0:10
Elapsed time: 5.373472143 seconds (6.0) for fibfp:35.0:10
+!CSVLINE!+bones-8,fibfp:35.0:10,5.373472143

real	0m5.375s
user	0m5.344s
sys	0m0.017s

Testing sum under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/sum.scm /tmp/r7rs-benchmarks/Bones/sum.exe
bones /tmp/r7rs-benchmarks/Bones/sum.scm > /tmp/r7rs-benchmarks/Bones/sum.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/sum.scm.s -o /tmp/r7rs-benchmarks/Bones/sum.scm.o
gcc /tmp/r7rs-benchmarks/Bones/sum.scm.o -o /tmp/r7rs-benchmarks/Bones/sum.exe
Running...
Running sum:10000:200000
Elapsed time: 30.813211182 seconds (30.0) for sum:10000:200000
+!CSVLINE!+bones-8,sum:10000:200000,30.813211182

real	0m30.814s
user	0m30.725s
sys	0m0.007s

Testing sumfp under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/sumfp.scm /tmp/r7rs-benchmarks/Bones/sumfp.exe
bones /tmp/r7rs-benchmarks/Bones/sumfp.scm > /tmp/r7rs-benchmarks/Bones/sumfp.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/sumfp.scm.s -o /tmp/r7rs-benchmarks/Bones/sumfp.scm.o
gcc /tmp/r7rs-benchmarks/Bones/sumfp.scm.o -o /tmp/r7rs-benchmarks/Bones/sumfp.exe
Running...
Running sumfp:1000000.0:500
Elapsed time: 9.183441946 seconds (10.0) for sumfp:1000000.0:500
+!CSVLINE!+bones-8,sumfp:1000000.0:500,9.183441946

real	0m9.185s
user	0m9.146s
sys	0m0.013s

Testing fft under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/fft.scm /tmp/r7rs-benchmarks/Bones/fft.exe
bones /tmp/r7rs-benchmarks/Bones/fft.scm > /tmp/r7rs-benchmarks/Bones/fft.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/fft.scm.s -o /tmp/r7rs-benchmarks/Bones/fft.scm.o
gcc /tmp/r7rs-benchmarks/Bones/fft.scm.o -o /tmp/r7rs-benchmarks/Bones/fft.exe
Running...
Running fft:65536:100
Elapsed time: 10.118855798 seconds (11.0) for fft:65536:100
+!CSVLINE!+bones-8,fft:65536:100,10.118855798

real	0m10.120s
user	0m10.091s
sys	0m0.003s

Testing mbrot under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/mbrot.scm /tmp/r7rs-benchmarks/Bones/mbrot.exe
bones /tmp/r7rs-benchmarks/Bones/mbrot.scm > /tmp/r7rs-benchmarks/Bones/mbrot.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/mbrot.scm.s -o /tmp/r7rs-benchmarks/Bones/mbrot.scm.o
gcc /tmp/r7rs-benchmarks/Bones/mbrot.scm.o -o /tmp/r7rs-benchmarks/Bones/mbrot.exe
Running...
Running mbrot:75:1000
Elapsed time: 12.154863209 seconds (12.0) for mbrot:75:1000
+!CSVLINE!+bones-8,mbrot:75:1000,12.154863209

real	0m12.156s
user	0m12.108s
sys	0m0.013s

Testing mbrotZ under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/mbrotZ.scm /tmp/r7rs-benchmarks/Bones/mbrotZ.exe
bones /tmp/r7rs-benchmarks/Bones/mbrotZ.scm > /tmp/r7rs-benchmarks/Bones/mbrotZ.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/mbrotZ.scm.s -o /tmp/r7rs-benchmarks/Bones/mbrotZ.scm.o
/tmp/r7rs-benchmarks/Bones/mbrotZ.scm.s:2491: error: symbol `____2d1_2e0_2d0_2e5i' not defined
/tmp/r7rs-benchmarks/Bones/mbrotZ.scm.s:3010: error: symbol `___make_2drectangular' not defined
/tmp/r7rs-benchmarks/Bones/mbrotZ.scm.s:3247: error: symbol `___real_2dpart' not defined
/tmp/r7rs-benchmarks/Bones/mbrotZ.scm.s:3277: error: symbol `___imag_2dpart' not defined
gcc /tmp/r7rs-benchmarks/Bones/mbrotZ.scm.o -o /tmp/r7rs-benchmarks/Bones/mbrotZ.exe
/usr/bin/ld: cannot find /tmp/r7rs-benchmarks/Bones/mbrotZ.scm.o: No such file or directory
collect2: error: ld returned 1 exit status
+!CSVLINE!+bones,mbrotZ,COMPILEERROR

Testing nucleic under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/nucleic.scm /tmp/r7rs-benchmarks/Bones/nucleic.exe
bones /tmp/r7rs-benchmarks/Bones/nucleic.scm > /tmp/r7rs-benchmarks/Bones/nucleic.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/nucleic.scm.s -o /tmp/r7rs-benchmarks/Bones/nucleic.scm.o
gcc /tmp/r7rs-benchmarks/Bones/nucleic.scm.o -o /tmp/r7rs-benchmarks/Bones/nucleic.exe
Running...
Running nucleic:50
Elapsed time: 6.632569543 seconds (6.0) for nucleic:50
+!CSVLINE!+bones-8,nucleic:50,6.632569543

real	0m6.634s
user	0m6.589s
sys	0m0.013s

Testing pi under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/pi.scm /tmp/r7rs-benchmarks/Bones/pi.exe
bones /tmp/r7rs-benchmarks/Bones/pi.scm > /tmp/r7rs-benchmarks/Bones/pi.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/pi.scm.s -o /tmp/r7rs-benchmarks/Bones/pi.scm.o
gcc /tmp/r7rs-benchmarks/Bones/pi.scm.o -o /tmp/r7rs-benchmarks/Bones/pi.exe
Running...
Running pi:50:500:50:100
./bench: line 596: 165241 Floating point exception(core dumped) "$1" < "$2"

real	0m0.204s
user	0m0.003s
sys	0m0.042s
+!CSVLINE!+bones,pi,CRASHED

Testing pnpoly under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/pnpoly.scm /tmp/r7rs-benchmarks/Bones/pnpoly.exe
bones /tmp/r7rs-benchmarks/Bones/pnpoly.scm > /tmp/r7rs-benchmarks/Bones/pnpoly.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/pnpoly.scm.s -o /tmp/r7rs-benchmarks/Bones/pnpoly.scm.o
gcc /tmp/r7rs-benchmarks/Bones/pnpoly.scm.o -o /tmp/r7rs-benchmarks/Bones/pnpoly.exe
Running...
Running pnpoly:1000000
Elapsed time: 12.801088465 seconds (12.0) for pnpoly:1000000
+!CSVLINE!+bones-8,pnpoly:1000000,12.801088465

real	0m12.802s
user	0m12.759s
sys	0m0.007s

Testing ray under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/ray.scm /tmp/r7rs-benchmarks/Bones/ray.exe
bones /tmp/r7rs-benchmarks/Bones/ray.scm > /tmp/r7rs-benchmarks/Bones/ray.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/ray.scm.s -o /tmp/r7rs-benchmarks/Bones/ray.scm.o
/tmp/r7rs-benchmarks/Bones/ray.scm.s:23709: error: invalid character in floating-point constant exponent: '.'
gcc /tmp/r7rs-benchmarks/Bones/ray.scm.o -o /tmp/r7rs-benchmarks/Bones/ray.exe
/usr/bin/ld: cannot find /tmp/r7rs-benchmarks/Bones/ray.scm.o: No such file or directory
collect2: error: ld returned 1 exit status
+!CSVLINE!+bones,ray,COMPILEERROR

Testing simplex under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/simplex.scm /tmp/r7rs-benchmarks/Bones/simplex.exe
bones /tmp/r7rs-benchmarks/Bones/simplex.scm > /tmp/r7rs-benchmarks/Bones/simplex.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/simplex.scm.s -o /tmp/r7rs-benchmarks/Bones/simplex.scm.o
gcc /tmp/r7rs-benchmarks/Bones/simplex.scm.o -o /tmp/r7rs-benchmarks/Bones/simplex.exe
Running...
Running simplex:1000000
Elapsed time: 10.586227764 seconds (10.0) for simplex:1000000
+!CSVLINE!+bones-8,simplex:1000000,10.586227764

real	0m10.587s
user	0m10.526s
sys	0m0.017s

Testing ack under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/ack.scm /tmp/r7rs-benchmarks/Bones/ack.exe
bones /tmp/r7rs-benchmarks/Bones/ack.scm > /tmp/r7rs-benchmarks/Bones/ack.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/ack.scm.s -o /tmp/r7rs-benchmarks/Bones/ack.scm.o
gcc /tmp/r7rs-benchmarks/Bones/ack.scm.o -o /tmp/r7rs-benchmarks/Bones/ack.exe
Running...
Running ack:3:12:2
Elapsed time: 22.106546034 seconds (22.0) for ack:3:12:2
+!CSVLINE!+bones-8,ack:3:12:2,22.106546034

real	0m22.108s
user	0m22.011s
sys	0m0.010s

Testing array1 under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/array1.scm /tmp/r7rs-benchmarks/Bones/array1.exe
bones /tmp/r7rs-benchmarks/Bones/array1.scm > /tmp/r7rs-benchmarks/Bones/array1.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/array1.scm.s -o /tmp/r7rs-benchmarks/Bones/array1.scm.o
gcc /tmp/r7rs-benchmarks/Bones/array1.scm.o -o /tmp/r7rs-benchmarks/Bones/array1.exe
Running...
Running array1:1000000:500
Elapsed time: 19.481472904 seconds (20.0) for array1:1000000:500
+!CSVLINE!+bones-8,array1:1000000:500,19.481472904

real	0m19.483s
user	0m19.428s
sys	0m0.003s

Testing string under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/string.scm /tmp/r7rs-benchmarks/Bones/string.exe
bones /tmp/r7rs-benchmarks/Bones/string.scm > /tmp/r7rs-benchmarks/Bones/string.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/string.scm.s -o /tmp/r7rs-benchmarks/Bones/string.scm.o
gcc /tmp/r7rs-benchmarks/Bones/string.scm.o -o /tmp/r7rs-benchmarks/Bones/string.exe
Running...
Running string:500000:100
Elapsed time: 0.266717311 seconds (0.0) for string:500000:100
+!CSVLINE!+bones-8,string:500000:100,0.266717311

real	0m0.268s
user	0m0.250s
sys	0m0.017s

Testing sum1 under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/sum1.scm /tmp/r7rs-benchmarks/Bones/sum1.exe
bones /tmp/r7rs-benchmarks/Bones/sum1.scm > /tmp/r7rs-benchmarks/Bones/sum1.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/sum1.scm.s -o /tmp/r7rs-benchmarks/Bones/sum1.scm.o
gcc /tmp/r7rs-benchmarks/Bones/sum1.scm.o -o /tmp/r7rs-benchmarks/Bones/sum1.exe
Running...
Running sum1:25
Elapsed time: 14.615903157 seconds (14.0) for sum1:25
+!CSVLINE!+bones-8,sum1:25,14.615903157

real	0m14.617s
user	0m7.686s
sys	0m6.917s

Testing cat under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/cat.scm /tmp/r7rs-benchmarks/Bones/cat.exe
bones /tmp/r7rs-benchmarks/Bones/cat.scm > /tmp/r7rs-benchmarks/Bones/cat.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/cat.scm.s -o /tmp/r7rs-benchmarks/Bones/cat.scm.o
gcc /tmp/r7rs-benchmarks/Bones/cat.scm.o -o /tmp/r7rs-benchmarks/Bones/cat.exe
Running...
Running cat:50
./bench: line 596: 165404 Killed                  "$1" < "$2"

real	5m0.231s
user	0m52.394s
sys	4m7.563s
+!CSVLINE!+bones,cat,ULIMITKILLED

Testing tail under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/tail.scm /tmp/r7rs-benchmarks/Bones/tail.exe
bones /tmp/r7rs-benchmarks/Bones/tail.scm > /tmp/r7rs-benchmarks/Bones/tail.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/tail.scm.s -o /tmp/r7rs-benchmarks/Bones/tail.scm.o
/tmp/r7rs-benchmarks/Bones/tail.scm.s:2657: error: symbol `___read_2dline' not defined
gcc /tmp/r7rs-benchmarks/Bones/tail.scm.o -o /tmp/r7rs-benchmarks/Bones/tail.exe
/usr/bin/ld: cannot find /tmp/r7rs-benchmarks/Bones/tail.scm.o: No such file or directory
collect2: error: ld returned 1 exit status
+!CSVLINE!+bones,tail,COMPILEERROR

Testing wc under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/wc.scm /tmp/r7rs-benchmarks/Bones/wc.exe
bones /tmp/r7rs-benchmarks/Bones/wc.scm > /tmp/r7rs-benchmarks/Bones/wc.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/wc.scm.s -o /tmp/r7rs-benchmarks/Bones/wc.scm.o
gcc /tmp/r7rs-benchmarks/Bones/wc.scm.o -o /tmp/r7rs-benchmarks/Bones/wc.exe
Running...
Running wc:inputs/bib:50
Elapsed time: 85.988985941 seconds (86.0) for wc:inputs/bib:50
+!CSVLINE!+bones-8,wc:inputs/bib:50,85.988985941

real	1m25.990s
user	0m29.519s
sys	0m56.401s

Testing read1 under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/read1.scm /tmp/r7rs-benchmarks/Bones/read1.exe
bones /tmp/r7rs-benchmarks/Bones/read1.scm > /tmp/r7rs-benchmarks/Bones/read1.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/read1.scm.s -o /tmp/r7rs-benchmarks/Bones/read1.scm.o
gcc /tmp/r7rs-benchmarks/Bones/read1.scm.o -o /tmp/r7rs-benchmarks/Bones/read1.exe
Running...
Running read1:2500
Elapsed time: 30.684457648 seconds (31.0) for read1:2500
+!CSVLINE!+bones-8,read1:2500,30.684457648

real	0m30.686s
user	0m12.726s
sys	0m17.934s

Testing compiler under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/compiler.scm /tmp/r7rs-benchmarks/Bones/compiler.exe
bones /tmp/r7rs-benchmarks/Bones/compiler.scm > /tmp/r7rs-benchmarks/Bones/compiler.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/compiler.scm.s -o /tmp/r7rs-benchmarks/Bones/compiler.scm.o
/tmp/r7rs-benchmarks/Bones/compiler.scm.s:76471: error: symbol `___real_2dpart' not defined
/tmp/r7rs-benchmarks/Bones/compiler.scm.s:76505: error: symbol `___imag_2dpart' not defined
/tmp/r7rs-benchmarks/Bones/compiler.scm.s:76613: error: symbol `___numerator' not defined
/tmp/r7rs-benchmarks/Bones/compiler.scm.s:76647: error: symbol `___denominator' not defined
/tmp/r7rs-benchmarks/Bones/compiler.scm.s:107356: error: symbol `___real_2dpart' not defined
/tmp/r7rs-benchmarks/Bones/compiler.scm.s:107405: error: symbol `___imag_2dpart' not defined
/tmp/r7rs-benchmarks/Bones/compiler.scm.s:107451: error: symbol `___imag_2dpart' not defined
gcc /tmp/r7rs-benchmarks/Bones/compiler.scm.o -o /tmp/r7rs-benchmarks/Bones/compiler.exe
/usr/bin/ld: cannot find /tmp/r7rs-benchmarks/Bones/compiler.scm.o: No such file or directory
collect2: error: ld returned 1 exit status
+!CSVLINE!+bones,compiler,COMPILEERROR

Testing conform under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/conform.scm /tmp/r7rs-benchmarks/Bones/conform.exe
bones /tmp/r7rs-benchmarks/Bones/conform.scm > /tmp/r7rs-benchmarks/Bones/conform.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/conform.scm.s -o /tmp/r7rs-benchmarks/Bones/conform.scm.o
gcc /tmp/r7rs-benchmarks/Bones/conform.scm.o -o /tmp/r7rs-benchmarks/Bones/conform.exe
Running...
Running conform:500
Elapsed time: 6.918740995 seconds (7.0) for conform:500
+!CSVLINE!+bones-8,conform:500,6.918740995

real	0m6.920s
user	0m6.890s
sys	0m0.010s

Testing dynamic under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/dynamic.scm /tmp/r7rs-benchmarks/Bones/dynamic.exe
bones /tmp/r7rs-benchmarks/Bones/dynamic.scm > /tmp/r7rs-benchmarks/Bones/dynamic.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/dynamic.scm.s -o /tmp/r7rs-benchmarks/Bones/dynamic.scm.o
gcc /tmp/r7rs-benchmarks/Bones/dynamic.scm.o -o /tmp/r7rs-benchmarks/Bones/dynamic.exe
Running...
Running dynamic:500
Elapsed time: 26.719817461 seconds (27.0) for dynamic:500
+!CSVLINE!+bones-8,dynamic:500,26.719817461

real	0m26.721s
user	0m16.156s
sys	0m10.530s

Testing earley under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/earley.scm /tmp/r7rs-benchmarks/Bones/earley.exe
bones /tmp/r7rs-benchmarks/Bones/earley.scm > /tmp/r7rs-benchmarks/Bones/earley.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/earley.scm.s -o /tmp/r7rs-benchmarks/Bones/earley.scm.o
gcc /tmp/r7rs-benchmarks/Bones/earley.scm.o -o /tmp/r7rs-benchmarks/Bones/earley.exe
Running...
Running earley:1

Error: out of memory

real	0m7.244s
user	0m7.220s
sys	0m0.003s
+!CSVLINE!+bones,earley,CRASHED

Testing graphs under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/graphs.scm /tmp/r7rs-benchmarks/Bones/graphs.exe
bones /tmp/r7rs-benchmarks/Bones/graphs.scm > /tmp/r7rs-benchmarks/Bones/graphs.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/graphs.scm.s -o /tmp/r7rs-benchmarks/Bones/graphs.scm.o
gcc /tmp/r7rs-benchmarks/Bones/graphs.scm.o -o /tmp/r7rs-benchmarks/Bones/graphs.exe
Running...
Running graphs:7:3
Elapsed time: 20.929240828 seconds (21.0) for graphs:7:3
+!CSVLINE!+bones-8,graphs:7:3,20.929240828

real	0m20.930s
user	0m20.861s
sys	0m0.013s

Testing lattice under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/lattice.scm /tmp/r7rs-benchmarks/Bones/lattice.exe
bones /tmp/r7rs-benchmarks/Bones/lattice.scm > /tmp/r7rs-benchmarks/Bones/lattice.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/lattice.scm.s -o /tmp/r7rs-benchmarks/Bones/lattice.scm.o
gcc /tmp/r7rs-benchmarks/Bones/lattice.scm.o -o /tmp/r7rs-benchmarks/Bones/lattice.exe
Running...
Running lattice:44:10
Elapsed time: 18.301312126 seconds (18.0) for lattice:44:10
+!CSVLINE!+bones-8,lattice:44:10,18.301312126

real	0m18.302s
user	0m18.236s
sys	0m0.017s

Testing matrix under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/matrix.scm /tmp/r7rs-benchmarks/Bones/matrix.exe
bones /tmp/r7rs-benchmarks/Bones/matrix.scm > /tmp/r7rs-benchmarks/Bones/matrix.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/matrix.scm.s -o /tmp/r7rs-benchmarks/Bones/matrix.scm.o
gcc /tmp/r7rs-benchmarks/Bones/matrix.scm.o -o /tmp/r7rs-benchmarks/Bones/matrix.exe
Running...
Running matrix:5:5:2500
Elapsed time: 5.20848575 seconds (6.0) for matrix:5:5:2500
+!CSVLINE!+bones-8,matrix:5:5:2500,5.20848575

real	0m5.210s
user	0m5.189s
sys	0m0.007s

Testing maze under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/maze.scm /tmp/r7rs-benchmarks/Bones/maze.exe
bones /tmp/r7rs-benchmarks/Bones/maze.scm > /tmp/r7rs-benchmarks/Bones/maze.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/maze.scm.s -o /tmp/r7rs-benchmarks/Bones/maze.scm.o
gcc /tmp/r7rs-benchmarks/Bones/maze.scm.o -o /tmp/r7rs-benchmarks/Bones/maze.exe
Running...
Running maze:20:7:10000
Elapsed time: 3.800621025 seconds (4.0) for maze:20:7:10000
+!CSVLINE!+bones-8,maze:20:7:10000,3.800621025

real	0m3.804s
user	0m3.782s
sys	0m0.010s

Testing mazefun under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/mazefun.scm /tmp/r7rs-benchmarks/Bones/mazefun.exe
bones /tmp/r7rs-benchmarks/Bones/mazefun.scm > /tmp/r7rs-benchmarks/Bones/mazefun.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/mazefun.scm.s -o /tmp/r7rs-benchmarks/Bones/mazefun.scm.o
gcc /tmp/r7rs-benchmarks/Bones/mazefun.scm.o -o /tmp/r7rs-benchmarks/Bones/mazefun.exe
Running...
Running mazefun:11:11:10000
Elapsed time: 9.514748158 seconds (9.0) for mazefun:11:11:10000
+!CSVLINE!+bones-8,mazefun:11:11:10000,9.514748158

real	0m9.516s
user	0m9.480s
sys	0m0.010s

Testing nqueens under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/nqueens.scm /tmp/r7rs-benchmarks/Bones/nqueens.exe
bones /tmp/r7rs-benchmarks/Bones/nqueens.scm > /tmp/r7rs-benchmarks/Bones/nqueens.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/nqueens.scm.s -o /tmp/r7rs-benchmarks/Bones/nqueens.scm.o
gcc /tmp/r7rs-benchmarks/Bones/nqueens.scm.o -o /tmp/r7rs-benchmarks/Bones/nqueens.exe
Running...
Running nqueens:13:10
Elapsed time: 29.300504937 seconds (30.0) for nqueens:13:10
+!CSVLINE!+bones-8,nqueens:13:10,29.300504937

real	0m29.302s
user	0m29.212s
sys	0m0.010s

Testing paraffins under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/paraffins.scm /tmp/r7rs-benchmarks/Bones/paraffins.exe
bones /tmp/r7rs-benchmarks/Bones/paraffins.scm > /tmp/r7rs-benchmarks/Bones/paraffins.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/paraffins.scm.s -o /tmp/r7rs-benchmarks/Bones/paraffins.scm.o
gcc /tmp/r7rs-benchmarks/Bones/paraffins.scm.o -o /tmp/r7rs-benchmarks/Bones/paraffins.exe
Running...
Running paraffins:23:10

Error: out of memory

real	0m0.962s
user	0m0.953s
sys	0m0.007s
+!CSVLINE!+bones,paraffins,CRASHED

Testing parsing under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/parsing.scm /tmp/r7rs-benchmarks/Bones/parsing.exe
bones /tmp/r7rs-benchmarks/Bones/parsing.scm > /tmp/r7rs-benchmarks/Bones/parsing.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/parsing.scm.s -o /tmp/r7rs-benchmarks/Bones/parsing.scm.o
gcc /tmp/r7rs-benchmarks/Bones/parsing.scm.o -o /tmp/r7rs-benchmarks/Bones/parsing.exe
Running...
Running parsing:2500
Elapsed time: 16.137285026 seconds (16.0) for parsing:2500
+!CSVLINE!+bones-8,parsing:2500,16.137285026

real	0m16.139s
user	0m16.069s
sys	0m0.027s

Testing peval under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/peval.scm /tmp/r7rs-benchmarks/Bones/peval.exe
bones /tmp/r7rs-benchmarks/Bones/peval.scm > /tmp/r7rs-benchmarks/Bones/peval.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/peval.scm.s -o /tmp/r7rs-benchmarks/Bones/peval.scm.o
gcc /tmp/r7rs-benchmarks/Bones/peval.scm.o -o /tmp/r7rs-benchmarks/Bones/peval.exe
Running...
Running peval:2000
Elapsed time: 4.126018346 seconds (4.0) for peval:2000
+!CSVLINE!+bones-8,peval:2000,4.126018346

real	0m4.128s
user	0m4.099s
sys	0m0.017s

Testing primes under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/primes.scm /tmp/r7rs-benchmarks/Bones/primes.exe
bones /tmp/r7rs-benchmarks/Bones/primes.scm > /tmp/r7rs-benchmarks/Bones/primes.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/primes.scm.s -o /tmp/r7rs-benchmarks/Bones/primes.scm.o
gcc /tmp/r7rs-benchmarks/Bones/primes.scm.o -o /tmp/r7rs-benchmarks/Bones/primes.exe
Running...
Running primes:1000:10000
Elapsed time: 1.993496423 seconds (2.0) for primes:1000:10000
+!CSVLINE!+bones-8,primes:1000:10000,1.993496423

real	0m1.995s
user	0m1.979s
sys	0m0.003s

Testing quicksort under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/quicksort.scm /tmp/r7rs-benchmarks/Bones/quicksort.exe
bones /tmp/r7rs-benchmarks/Bones/quicksort.scm > /tmp/r7rs-benchmarks/Bones/quicksort.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/quicksort.scm.s -o /tmp/r7rs-benchmarks/Bones/quicksort.scm.o
/tmp/r7rs-benchmarks/Bones/quicksort.scm.s:2898: error: symbol `___vector_2dmap' not defined
gcc /tmp/r7rs-benchmarks/Bones/quicksort.scm.o -o /tmp/r7rs-benchmarks/Bones/quicksort.exe
/usr/bin/ld: cannot find /tmp/r7rs-benchmarks/Bones/quicksort.scm.o: No such file or directory
collect2: error: ld returned 1 exit status
+!CSVLINE!+bones,quicksort,COMPILEERROR

Testing scheme under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/scheme.scm /tmp/r7rs-benchmarks/Bones/scheme.exe
bones /tmp/r7rs-benchmarks/Bones/scheme.scm > /tmp/r7rs-benchmarks/Bones/scheme.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/scheme.scm.s -o /tmp/r7rs-benchmarks/Bones/scheme.scm.o
/tmp/r7rs-benchmarks/Bones/scheme.scm.s:3609: error: symbol `___complex_3f' not defined
gcc /tmp/r7rs-benchmarks/Bones/scheme.scm.o -o /tmp/r7rs-benchmarks/Bones/scheme.exe
/usr/bin/ld: cannot find /tmp/r7rs-benchmarks/Bones/scheme.scm.o: No such file or directory
collect2: error: ld returned 1 exit status
+!CSVLINE!+bones,scheme,COMPILEERROR

Testing slatex under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/slatex.scm /tmp/r7rs-benchmarks/Bones/slatex.exe
bones /tmp/r7rs-benchmarks/Bones/slatex.scm > /tmp/r7rs-benchmarks/Bones/slatex.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/slatex.scm.s -o /tmp/r7rs-benchmarks/Bones/slatex.scm.o
gcc /tmp/r7rs-benchmarks/Bones/slatex.scm.o -o /tmp/r7rs-benchmarks/Bones/slatex.exe
Running...
Running slatex:500
Elapsed time: 13.696666707 seconds (14.0) for slatex:500
+!CSVLINE!+bones-8,slatex:500,13.696666707

real	0m13.698s
user	0m4.798s
sys	0m8.846s

Testing chudnovsky under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/chudnovsky.scm /tmp/r7rs-benchmarks/Bones/chudnovsky.exe
bones /tmp/r7rs-benchmarks/Bones/chudnovsky.scm > /tmp/r7rs-benchmarks/Bones/chudnovsky.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/chudnovsky.scm.s -o /tmp/r7rs-benchmarks/Bones/chudnovsky.scm.o
gcc /tmp/r7rs-benchmarks/Bones/chudnovsky.scm.o -o /tmp/r7rs-benchmarks/Bones/chudnovsky.exe
Running...
Running chudnovsky:50:500:50:1000
ERROR: returned incorrect result: (0 0 0 0 0 0 0 0 0 0)
+!CSVLINE!+bones-8,chudnovsky:50:500:50:1000,INCORRECT

real	0m0.056s
user	0m0.029s
sys	0m0.026s

Testing nboyer under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/nboyer.scm /tmp/r7rs-benchmarks/Bones/nboyer.exe
bones /tmp/r7rs-benchmarks/Bones/nboyer.scm > /tmp/r7rs-benchmarks/Bones/nboyer.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/nboyer.scm.s -o /tmp/r7rs-benchmarks/Bones/nboyer.scm.o
gcc /tmp/r7rs-benchmarks/Bones/nboyer.scm.o -o /tmp/r7rs-benchmarks/Bones/nboyer.exe
Running...
Running nboyer:5:1

Error: out of memory

real	0m4.885s
user	0m4.852s
sys	0m0.013s
+!CSVLINE!+bones,nboyer,CRASHED

Testing sboyer under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/sboyer.scm /tmp/r7rs-benchmarks/Bones/sboyer.exe
bones /tmp/r7rs-benchmarks/Bones/sboyer.scm > /tmp/r7rs-benchmarks/Bones/sboyer.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/sboyer.scm.s -o /tmp/r7rs-benchmarks/Bones/sboyer.scm.o
gcc /tmp/r7rs-benchmarks/Bones/sboyer.scm.o -o /tmp/r7rs-benchmarks/Bones/sboyer.exe
Running...
Running sboyer:5:1

Error: out of memory

real	0m33.028s
user	0m32.916s
sys	0m0.020s
+!CSVLINE!+bones,sboyer,CRASHED

Testing gcbench under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/gcbench.scm /tmp/r7rs-benchmarks/Bones/gcbench.exe
bones /tmp/r7rs-benchmarks/Bones/gcbench.scm > /tmp/r7rs-benchmarks/Bones/gcbench.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/gcbench.scm.s -o /tmp/r7rs-benchmarks/Bones/gcbench.scm.o
/tmp/r7rs-benchmarks/Bones/gcbench.scm.s:3032: error: symbol `___make_2dnode_2draw' not defined
/tmp/r7rs-benchmarks/Bones/gcbench.scm.s:3033: error: symbol `___left' not defined
/tmp/r7rs-benchmarks/Bones/gcbench.scm.s:3034: error: symbol `___right' not defined
/tmp/r7rs-benchmarks/Bones/gcbench.scm.s:3035: error: symbol `___i' not defined
/tmp/r7rs-benchmarks/Bones/gcbench.scm.s:3036: error: symbol `___j' not defined
/tmp/r7rs-benchmarks/Bones/gcbench.scm.s:3064: error: symbol `___left' not defined
/tmp/r7rs-benchmarks/Bones/gcbench.scm.s:3065: error: symbol `___node_2eleft' not defined
/tmp/r7rs-benchmarks/Bones/gcbench.scm.s:3066: error: symbol `___node_2eleft_2dset_21' not defined
/tmp/r7rs-benchmarks/Bones/gcbench.scm.s:3098: error: symbol `___right' not defined
/tmp/r7rs-benchmarks/Bones/gcbench.scm.s:3099: error: symbol `___node_2eright' not defined
/tmp/r7rs-benchmarks/Bones/gcbench.scm.s:3100: error: symbol `___node_2eright_2dset_21' not defined
/tmp/r7rs-benchmarks/Bones/gcbench.scm.s:3134: error: symbol `___i' not defined
/tmp/r7rs-benchmarks/Bones/gcbench.scm.s:3135: error: symbol `___node_2ei' not defined
/tmp/r7rs-benchmarks/Bones/gcbench.scm.s:3136: error: symbol `___node_2ei_2dset_21' not defined
/tmp/r7rs-benchmarks/Bones/gcbench.scm.s:3172: error: symbol `___j' not defined
/tmp/r7rs-benchmarks/Bones/gcbench.scm.s:3173: error: symbol `___node_2ej' not defined
/tmp/r7rs-benchmarks/Bones/gcbench.scm.s:3174: error: symbol `___node_2ej_2dset_21' not defined
/tmp/r7rs-benchmarks/Bones/gcbench.scm.s:3184: error: symbol `___class_4eode' not defined
/tmp/r7rs-benchmarks/Bones/gcbench.scm.s:3186: error: symbol `___class_4eode_3f' not defined
/tmp/r7rs-benchmarks/Bones/gcbench.scm.s:3208: error: symbol `___define_2drecord_2dtype' not defined
/tmp/r7rs-benchmarks/Bones/gcbench.scm.s:5680: error: symbol `___node_2eleft_2dset_21' not defined
/tmp/r7rs-benchmarks/Bones/gcbench.scm.s:5730: error: symbol `___node_2eright_2dset_21' not defined
/tmp/r7rs-benchmarks/Bones/gcbench.scm.s:5752: error: symbol `___node_2eleft' not defined
/tmp/r7rs-benchmarks/Bones/gcbench.scm.s:5805: error: symbol `___node_2eright' not defined
/tmp/r7rs-benchmarks/Bones/gcbench.scm.s:5835: error: symbol `___make_2dnode_2draw' not defined
/tmp/r7rs-benchmarks/Bones/gcbench.scm.s:5849: error: symbol `___make_2dnode_2draw' not defined
gcc /tmp/r7rs-benchmarks/Bones/gcbench.scm.o -o /tmp/r7rs-benchmarks/Bones/gcbench.exe
/usr/bin/ld: cannot find /tmp/r7rs-benchmarks/Bones/gcbench.scm.o: No such file or directory
collect2: error: ld returned 1 exit status
+!CSVLINE!+bones,gcbench,COMPILEERROR

Testing mperm under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/mperm.scm /tmp/r7rs-benchmarks/Bones/mperm.exe
bones /tmp/r7rs-benchmarks/Bones/mperm.scm > /tmp/r7rs-benchmarks/Bones/mperm.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/mperm.scm.s -o /tmp/r7rs-benchmarks/Bones/mperm.scm.o
gcc /tmp/r7rs-benchmarks/Bones/mperm.scm.o -o /tmp/r7rs-benchmarks/Bones/mperm.exe
Running...

Error: out of memory

real	0m2.519s
user	0m2.495s
sys	0m0.017s
+!CSVLINE!+bones,mperm,CRASHED

Testing equal under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/equal.scm /tmp/r7rs-benchmarks/Bones/equal.exe
bones /tmp/r7rs-benchmarks/Bones/equal.scm > /tmp/r7rs-benchmarks/Bones/equal.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/equal.scm.s -o /tmp/r7rs-benchmarks/Bones/equal.scm.o
gcc /tmp/r7rs-benchmarks/Bones/equal.scm.o -o /tmp/r7rs-benchmarks/Bones/equal.exe
Running...
Running equal:100:100:8:1000:2000:5000
./bench: line 596: 166048 Killed                  "$1" < "$2"

real	5m0.103s
user	4m59.995s
sys	0m0.000s
+!CSVLINE!+bones,equal,ULIMITKILLED

Testing bv2string under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/r7rs-benchmarks/Bones/bv2string.scm /tmp/r7rs-benchmarks/Bones/bv2string.exe
bones /tmp/r7rs-benchmarks/Bones/bv2string.scm > /tmp/r7rs-benchmarks/Bones/bv2string.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/r7rs-benchmarks/Bones/bv2string.scm.s -o /tmp/r7rs-benchmarks/Bones/bv2string.scm.o
/tmp/r7rs-benchmarks/Bones/bv2string.scm.s:3019: error: symbol `___utf8_2d_3estring' not defined
/tmp/r7rs-benchmarks/Bones/bv2string.scm.s:3020: error: symbol `___string_2d_3eutf8' not defined
/tmp/r7rs-benchmarks/Bones/bv2string.scm.s:3163: error: symbol `___utf8_2d_3estring' not defined
/tmp/r7rs-benchmarks/Bones/bv2string.scm.s:3164: error: symbol `___string_2d_3eutf8' not defined
gcc /tmp/r7rs-benchmarks/Bones/bv2string.scm.o -o /tmp/r7rs-benchmarks/Bones/bv2string.exe
/usr/bin/ld: cannot find /tmp/r7rs-benchmarks/Bones/bv2string.scm.o: No such file or directory
collect2: error: ld returned 1 exit status
+!CSVLINE!+bones,bv2string,COMPILEERROR
