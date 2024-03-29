
****************************
Benchmarking Bones on Sat 18 Jun 05:54:12 UTC 2022 under Linux benchmarks 5.4.157-1-pve #1 SMP PVE 5.4.157-1 (Mon, 29 Nov 2021 12:01:44 +0100) x86_64 GNU/Linux

Testing browse under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/browse.scm /tmp/larcenous/Bones/browse.exe
bones /tmp/larcenous/Bones/browse.scm > /tmp/larcenous/Bones/browse.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/browse.scm.s -o /tmp/larcenous/Bones/browse.scm.o
gcc /tmp/larcenous/Bones/browse.scm.o -o /tmp/larcenous/Bones/browse.exe
Running...
Running browse:2000
Elapsed time: 0.549254837 seconds (1.0) for browse:2000
+!CSVLINE!+bones-8,browse:2000,0.549254837

real	0m0.555s
user	0m0.547s
sys	0m0.008s

Testing deriv under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/deriv.scm /tmp/larcenous/Bones/deriv.exe
bones /tmp/larcenous/Bones/deriv.scm > /tmp/larcenous/Bones/deriv.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/deriv.scm.s -o /tmp/larcenous/Bones/deriv.scm.o
gcc /tmp/larcenous/Bones/deriv.scm.o -o /tmp/larcenous/Bones/deriv.exe
Running...
Running deriv:10000000
Elapsed time: 3.458553105 seconds (3.0) for deriv:10000000
+!CSVLINE!+bones-8,deriv:10000000,3.458553105

real	0m3.463s
user	0m3.447s
sys	0m0.016s

Testing destruc under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/destruc.scm /tmp/larcenous/Bones/destruc.exe
bones /tmp/larcenous/Bones/destruc.scm > /tmp/larcenous/Bones/destruc.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/destruc.scm.s -o /tmp/larcenous/Bones/destruc.scm.o
gcc /tmp/larcenous/Bones/destruc.scm.o -o /tmp/larcenous/Bones/destruc.exe
Running...
Running destruc:600:50:4000
Elapsed time: 7.870931761 seconds (8.0) for destruc:600:50:4000
+!CSVLINE!+bones-8,destruc:600:50:4000,7.870931761

real	0m7.875s
user	0m7.847s
sys	0m0.028s

Testing diviter under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/diviter.scm /tmp/larcenous/Bones/diviter.exe
bones /tmp/larcenous/Bones/diviter.scm > /tmp/larcenous/Bones/diviter.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/diviter.scm.s -o /tmp/larcenous/Bones/diviter.scm.o
gcc /tmp/larcenous/Bones/diviter.scm.o -o /tmp/larcenous/Bones/diviter.exe
Running...
Running diviter:1000:1000000
Elapsed time: 2.080881426 seconds (2.0) for diviter:1000:1000000
+!CSVLINE!+bones-8,diviter:1000:1000000,2.080881426

real	0m2.085s
user	0m2.065s
sys	0m0.020s

Testing divrec under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/divrec.scm /tmp/larcenous/Bones/divrec.exe
bones /tmp/larcenous/Bones/divrec.scm > /tmp/larcenous/Bones/divrec.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/divrec.scm.s -o /tmp/larcenous/Bones/divrec.scm.o
gcc /tmp/larcenous/Bones/divrec.scm.o -o /tmp/larcenous/Bones/divrec.exe
Running...
Running divrec:1000:1000000
Elapsed time: 4.078992818 seconds (4.0) for divrec:1000:1000000
+!CSVLINE!+bones-8,divrec:1000:1000000,4.078992818

real	0m4.083s
user	0m4.063s
sys	0m0.020s

Testing puzzle under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/puzzle.scm /tmp/larcenous/Bones/puzzle.exe
bones /tmp/larcenous/Bones/puzzle.scm > /tmp/larcenous/Bones/puzzle.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/puzzle.scm.s -o /tmp/larcenous/Bones/puzzle.scm.o
gcc /tmp/larcenous/Bones/puzzle.scm.o -o /tmp/larcenous/Bones/puzzle.exe
Running...
Running puzzle:1000
Elapsed time: 12.883804323 seconds (13.0) for puzzle:1000
+!CSVLINE!+bones-8,puzzle:1000,12.883804323

real	0m12.888s
user	0m12.871s
sys	0m0.016s

Testing triangl under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/triangl.scm /tmp/larcenous/Bones/triangl.exe
bones /tmp/larcenous/Bones/triangl.scm > /tmp/larcenous/Bones/triangl.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/triangl.scm.s -o /tmp/larcenous/Bones/triangl.scm.o
gcc /tmp/larcenous/Bones/triangl.scm.o -o /tmp/larcenous/Bones/triangl.exe
Running...
Running triangl:22:1:50
Elapsed time: 9.702724999999999 seconds (10.0) for triangl:22:1:50
+!CSVLINE!+bones-8,triangl:22:1:50,9.702724999999999

real	0m9.707s
user	0m9.690s
sys	0m0.016s

Testing tak under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/tak.scm /tmp/larcenous/Bones/tak.exe
bones /tmp/larcenous/Bones/tak.scm > /tmp/larcenous/Bones/tak.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/tak.scm.s -o /tmp/larcenous/Bones/tak.scm.o
gcc /tmp/larcenous/Bones/tak.scm.o -o /tmp/larcenous/Bones/tak.exe
Running...
Running tak:40:20:11:1
Elapsed time: 10.114506145 seconds (10.0) for tak:40:20:11:1
+!CSVLINE!+bones-8,tak:40:20:11:1,10.114506145

real	0m10.118s
user	0m10.090s
sys	0m0.028s

Testing takl under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/takl.scm /tmp/larcenous/Bones/takl.exe
bones /tmp/larcenous/Bones/takl.scm > /tmp/larcenous/Bones/takl.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/takl.scm.s -o /tmp/larcenous/Bones/takl.scm.o
gcc /tmp/larcenous/Bones/takl.scm.o -o /tmp/larcenous/Bones/takl.exe
Running...
Running takl:40:20:12:1
Elapsed time: 7.138987999 seconds (7.0) for takl:40:20:12:1
+!CSVLINE!+bones-8,takl:40:20:12:1,7.138987999

real	0m7.143s
user	0m7.113s
sys	0m0.028s

Testing ntakl under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/ntakl.scm /tmp/larcenous/Bones/ntakl.exe
bones /tmp/larcenous/Bones/ntakl.scm > /tmp/larcenous/Bones/ntakl.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/ntakl.scm.s -o /tmp/larcenous/Bones/ntakl.scm.o
gcc /tmp/larcenous/Bones/ntakl.scm.o -o /tmp/larcenous/Bones/ntakl.exe
Running...
Running ntakl:40:20:12:1
Elapsed time: 6.725227585 seconds (6.0) for ntakl:40:20:12:1
+!CSVLINE!+bones-8,ntakl:40:20:12:1,6.725227585

real	0m6.730s
user	0m6.713s
sys	0m0.016s

Testing cpstak under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/cpstak.scm /tmp/larcenous/Bones/cpstak.exe
bones /tmp/larcenous/Bones/cpstak.scm > /tmp/larcenous/Bones/cpstak.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/cpstak.scm.s -o /tmp/larcenous/Bones/cpstak.scm.o
gcc /tmp/larcenous/Bones/cpstak.scm.o -o /tmp/larcenous/Bones/cpstak.exe
Running...
Running cpstak:40:20:11:1
Elapsed time: 13.186616049 seconds (14.0) for cpstak:40:20:11:1
+!CSVLINE!+bones-8,cpstak:40:20:11:1,13.186616049

real	0m13.190s
user	0m13.178s
sys	0m0.012s

Testing ctak under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/ctak.scm /tmp/larcenous/Bones/ctak.exe
bones /tmp/larcenous/Bones/ctak.scm > /tmp/larcenous/Bones/ctak.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/ctak.scm.s -o /tmp/larcenous/Bones/ctak.scm.o
gcc /tmp/larcenous/Bones/ctak.scm.o -o /tmp/larcenous/Bones/ctak.exe
Running...
Running ctak:32:16:8:1
Elapsed time: 1.587938139 seconds (1.0) for ctak:32:16:8:1
+!CSVLINE!+bones-8,ctak:32:16:8:1,1.587938139

real	0m1.592s
user	0m1.568s
sys	0m0.024s

Testing fib under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/fib.scm /tmp/larcenous/Bones/fib.exe
bones /tmp/larcenous/Bones/fib.scm > /tmp/larcenous/Bones/fib.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/fib.scm.s -o /tmp/larcenous/Bones/fib.scm.o
gcc /tmp/larcenous/Bones/fib.scm.o -o /tmp/larcenous/Bones/fib.exe
Running...
Running fib:40:5
Elapsed time: 23.853025945 seconds (23.0) for fib:40:5
+!CSVLINE!+bones-8,fib:40:5,23.853025945

real	0m23.857s
user	0m23.841s
sys	0m0.016s

Testing fibc under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/fibc.scm /tmp/larcenous/Bones/fibc.exe
bones /tmp/larcenous/Bones/fibc.scm > /tmp/larcenous/Bones/fibc.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/fibc.scm.s -o /tmp/larcenous/Bones/fibc.scm.o
gcc /tmp/larcenous/Bones/fibc.scm.o -o /tmp/larcenous/Bones/fibc.exe
Running...
Running fibc:30:10
Elapsed time: 1.796339838 seconds (1.0) for fibc:30:10
+!CSVLINE!+bones-8,fibc:30:10,1.796339838

real	0m1.800s
user	0m1.780s
sys	0m0.020s

Testing fibfp under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/fibfp.scm /tmp/larcenous/Bones/fibfp.exe
bones /tmp/larcenous/Bones/fibfp.scm > /tmp/larcenous/Bones/fibfp.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/fibfp.scm.s -o /tmp/larcenous/Bones/fibfp.scm.o
gcc /tmp/larcenous/Bones/fibfp.scm.o -o /tmp/larcenous/Bones/fibfp.exe
Running...
Running fibfp:35.0:10
Elapsed time: 5.18928066 seconds (6.0) for fibfp:35.0:10
+!CSVLINE!+bones-8,fibfp:35.0:10,5.18928066

real	0m5.193s
user	0m5.173s
sys	0m0.020s

Testing sum under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/sum.scm /tmp/larcenous/Bones/sum.exe
bones /tmp/larcenous/Bones/sum.scm > /tmp/larcenous/Bones/sum.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/sum.scm.s -o /tmp/larcenous/Bones/sum.scm.o
gcc /tmp/larcenous/Bones/sum.scm.o -o /tmp/larcenous/Bones/sum.exe
Running...
Running sum:10000:200000
Elapsed time: 32.46077009 seconds (32.0) for sum:10000:200000
+!CSVLINE!+bones-8,sum:10000:200000,32.46077009

real	0m32.465s
user	0m32.444s
sys	0m0.020s

Testing sumfp under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/sumfp.scm /tmp/larcenous/Bones/sumfp.exe
bones /tmp/larcenous/Bones/sumfp.scm > /tmp/larcenous/Bones/sumfp.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/sumfp.scm.s -o /tmp/larcenous/Bones/sumfp.scm.o
gcc /tmp/larcenous/Bones/sumfp.scm.o -o /tmp/larcenous/Bones/sumfp.exe
Running...
Running sumfp:1000000.0:500
Elapsed time: 9.770712133 seconds (10.0) for sumfp:1000000.0:500
+!CSVLINE!+bones-8,sumfp:1000000.0:500,9.770712133

real	0m9.775s
user	0m9.754s
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
Elapsed time: 10.844776911 seconds (11.0) for fft:65536:100
+!CSVLINE!+bones-8,fft:65536:100,10.844776911

real	0m10.849s
user	0m10.828s
sys	0m0.020s

Testing mbrot under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/mbrot.scm /tmp/larcenous/Bones/mbrot.exe
bones /tmp/larcenous/Bones/mbrot.scm > /tmp/larcenous/Bones/mbrot.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/mbrot.scm.s -o /tmp/larcenous/Bones/mbrot.scm.o
gcc /tmp/larcenous/Bones/mbrot.scm.o -o /tmp/larcenous/Bones/mbrot.exe
Running...
Running mbrot:75:1000
Elapsed time: 16.642294223 seconds (17.0) for mbrot:75:1000
+!CSVLINE!+bones-8,mbrot:75:1000,16.642294223

real	0m16.646s
user	0m16.638s
sys	0m0.008s

Testing mbrotZ under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/mbrotZ.scm /tmp/larcenous/Bones/mbrotZ.exe
bones /tmp/larcenous/Bones/mbrotZ.scm > /tmp/larcenous/Bones/mbrotZ.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/mbrotZ.scm.s -o /tmp/larcenous/Bones/mbrotZ.scm.o
/tmp/larcenous/Bones/mbrotZ.scm.s:2286: error: symbol `____2d1_2e0_2d0_2e5i' not defined
/tmp/larcenous/Bones/mbrotZ.scm.s:2805: error: symbol `___make_2drectangular' not defined
/tmp/larcenous/Bones/mbrotZ.scm.s:3042: error: symbol `___real_2dpart' not defined
/tmp/larcenous/Bones/mbrotZ.scm.s:3072: error: symbol `___imag_2dpart' not defined
gcc /tmp/larcenous/Bones/mbrotZ.scm.o -o /tmp/larcenous/Bones/mbrotZ.exe
/sbin/ld: cannot find /tmp/larcenous/Bones/mbrotZ.scm.o: No such file or directory
collect2: error: ld returned 1 exit status
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
Elapsed time: 6.460634387 seconds (7.0) for nucleic:50
+!CSVLINE!+bones-8,nucleic:50,6.460634387

real	0m6.465s
user	0m6.452s
sys	0m0.012s

Testing pi under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/pi.scm /tmp/larcenous/Bones/pi.exe
bones /tmp/larcenous/Bones/pi.scm > /tmp/larcenous/Bones/pi.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/pi.scm.s -o /tmp/larcenous/Bones/pi.scm.o
gcc /tmp/larcenous/Bones/pi.scm.o -o /tmp/larcenous/Bones/pi.exe
Running...
Running pi:50:500:50:100
./bench: line 611:  8735 Floating point exception"$1" < "$2"

real	0m0.006s
user	0m0.000s
sys	0m0.006s
+!CSVLINE!+bones,pi,CRASHED

Testing pnpoly under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/pnpoly.scm /tmp/larcenous/Bones/pnpoly.exe
bones /tmp/larcenous/Bones/pnpoly.scm > /tmp/larcenous/Bones/pnpoly.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/pnpoly.scm.s -o /tmp/larcenous/Bones/pnpoly.scm.o
gcc /tmp/larcenous/Bones/pnpoly.scm.o -o /tmp/larcenous/Bones/pnpoly.exe
Running...
Running pnpoly:1000000
Elapsed time: 12.945149244 seconds (13.0) for pnpoly:1000000
+!CSVLINE!+bones-8,pnpoly:1000000,12.945149244

real	0m12.950s
user	0m12.945s
sys	0m0.004s

Testing ray under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/ray.scm /tmp/larcenous/Bones/ray.exe
bones /tmp/larcenous/Bones/ray.scm > /tmp/larcenous/Bones/ray.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/ray.scm.s -o /tmp/larcenous/Bones/ray.scm.o
/tmp/larcenous/Bones/ray.scm.s:23504: error: invalid character in floating-point constant exponent: '.'
gcc /tmp/larcenous/Bones/ray.scm.o -o /tmp/larcenous/Bones/ray.exe
/sbin/ld: cannot find /tmp/larcenous/Bones/ray.scm.o: No such file or directory
collect2: error: ld returned 1 exit status
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
Elapsed time: 9.78089662 seconds (10.0) for simplex:1000000
+!CSVLINE!+bones-8,simplex:1000000,9.78089662

real	0m9.785s
user	0m9.757s
sys	0m0.028s

Testing ack under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/ack.scm /tmp/larcenous/Bones/ack.exe
bones /tmp/larcenous/Bones/ack.scm > /tmp/larcenous/Bones/ack.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/ack.scm.s -o /tmp/larcenous/Bones/ack.scm.o
gcc /tmp/larcenous/Bones/ack.scm.o -o /tmp/larcenous/Bones/ack.exe
Running...
Running ack:3:12:2
Elapsed time: 33.026032613 seconds (33.0) for ack:3:12:2
+!CSVLINE!+bones-8,ack:3:12:2,33.026032613

real	0m33.030s
user	0m33.005s
sys	0m0.024s

Testing array1 under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/array1.scm /tmp/larcenous/Bones/array1.exe
bones /tmp/larcenous/Bones/array1.scm > /tmp/larcenous/Bones/array1.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/array1.scm.s -o /tmp/larcenous/Bones/array1.scm.o
gcc /tmp/larcenous/Bones/array1.scm.o -o /tmp/larcenous/Bones/array1.exe
Running...
Running array1:1000000:500
Elapsed time: 19.160629043 seconds (19.0) for array1:1000000:500
+!CSVLINE!+bones-8,array1:1000000:500,19.160629043

real	0m19.165s
user	0m19.140s
sys	0m0.024s

Testing string under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/string.scm /tmp/larcenous/Bones/string.exe
bones /tmp/larcenous/Bones/string.scm > /tmp/larcenous/Bones/string.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/string.scm.s -o /tmp/larcenous/Bones/string.scm.o
gcc /tmp/larcenous/Bones/string.scm.o -o /tmp/larcenous/Bones/string.exe
Running...
Running string:500000:100
Elapsed time: 0.249207274 seconds (1.0) for string:500000:100
+!CSVLINE!+bones-8,string:500000:100,0.249207274

real	0m0.253s
user	0m0.233s
sys	0m0.020s

Testing sum1 under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/sum1.scm /tmp/larcenous/Bones/sum1.exe
bones /tmp/larcenous/Bones/sum1.scm > /tmp/larcenous/Bones/sum1.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/sum1.scm.s -o /tmp/larcenous/Bones/sum1.scm.o
gcc /tmp/larcenous/Bones/sum1.scm.o -o /tmp/larcenous/Bones/sum1.exe
Running...
Running sum1:25
Elapsed time: 43.409327685 seconds (43.0) for sum1:25
+!CSVLINE!+bones-8,sum1:25,43.409327685

real	0m43.413s
user	0m8.598s
sys	0m34.800s

Testing cat under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/cat.scm /tmp/larcenous/Bones/cat.exe
bones /tmp/larcenous/Bones/cat.scm > /tmp/larcenous/Bones/cat.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/cat.scm.s -o /tmp/larcenous/Bones/cat.scm.o
gcc /tmp/larcenous/Bones/cat.scm.o -o /tmp/larcenous/Bones/cat.exe
Running...
Running cat:50
./bench: line 611:  8833 Killed                  "$1" < "$2"

real	5m0.001s
user	0m10.784s
sys	4m49.206s
+!CSVLINE!+bones,cat,ULIMITKILLED

Testing tail under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/tail.scm /tmp/larcenous/Bones/tail.exe
bones /tmp/larcenous/Bones/tail.scm > /tmp/larcenous/Bones/tail.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/tail.scm.s -o /tmp/larcenous/Bones/tail.scm.o
/tmp/larcenous/Bones/tail.scm.s:2452: error: symbol `___read_2dline' not defined
gcc /tmp/larcenous/Bones/tail.scm.o -o /tmp/larcenous/Bones/tail.exe
/sbin/ld: cannot find /tmp/larcenous/Bones/tail.scm.o: No such file or directory
collect2: error: ld returned 1 exit status
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
./bench: line 611:  8865 Killed                  "$1" < "$2"

real	5m0.002s
user	0m17.908s
sys	4m42.088s
+!CSVLINE!+bones,wc,ULIMITKILLED

Testing read1 under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/read1.scm /tmp/larcenous/Bones/read1.exe
bones /tmp/larcenous/Bones/read1.scm > /tmp/larcenous/Bones/read1.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/read1.scm.s -o /tmp/larcenous/Bones/read1.scm.o
gcc /tmp/larcenous/Bones/read1.scm.o -o /tmp/larcenous/Bones/read1.exe
Running...
Running read1:2500
Elapsed time: 96.42156744499999 seconds (96.0) for read1:2500
+!CSVLINE!+bones-8,read1:2500,96.42156744499999

real	1m36.426s
user	0m11.428s
sys	1m24.996s

Testing compiler under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/compiler.scm /tmp/larcenous/Bones/compiler.exe
bones /tmp/larcenous/Bones/compiler.scm > /tmp/larcenous/Bones/compiler.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/compiler.scm.s -o /tmp/larcenous/Bones/compiler.scm.o
/tmp/larcenous/Bones/compiler.scm.s:76266: error: symbol `___real_2dpart' not defined
/tmp/larcenous/Bones/compiler.scm.s:76300: error: symbol `___imag_2dpart' not defined
/tmp/larcenous/Bones/compiler.scm.s:76408: error: symbol `___numerator' not defined
/tmp/larcenous/Bones/compiler.scm.s:76442: error: symbol `___denominator' not defined
/tmp/larcenous/Bones/compiler.scm.s:107151: error: symbol `___real_2dpart' not defined
/tmp/larcenous/Bones/compiler.scm.s:107200: error: symbol `___imag_2dpart' not defined
/tmp/larcenous/Bones/compiler.scm.s:107246: error: symbol `___imag_2dpart' not defined
gcc /tmp/larcenous/Bones/compiler.scm.o -o /tmp/larcenous/Bones/compiler.exe
/sbin/ld: cannot find /tmp/larcenous/Bones/compiler.scm.o: No such file or directory
collect2: error: ld returned 1 exit status
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
Elapsed time: 7.172264976 seconds (8.0) for conform:500
+!CSVLINE!+bones-8,conform:500,7.172264976

real	0m7.177s
user	0m7.160s
sys	0m0.016s

Testing dynamic under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/dynamic.scm /tmp/larcenous/Bones/dynamic.exe
bones /tmp/larcenous/Bones/dynamic.scm > /tmp/larcenous/Bones/dynamic.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/dynamic.scm.s -o /tmp/larcenous/Bones/dynamic.scm.o
gcc /tmp/larcenous/Bones/dynamic.scm.o -o /tmp/larcenous/Bones/dynamic.exe
Running...
Running dynamic:500
Elapsed time: 67.88427846899999 seconds (68.0) for dynamic:500
+!CSVLINE!+bones-8,dynamic:500,67.88427846899999

real	1m7.889s
user	0m16.736s
sys	0m51.152s

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

real	0m7.196s
user	0m7.172s
sys	0m0.024s
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
Elapsed time: 23.486493113 seconds (24.0) for graphs:7:3
+!CSVLINE!+bones-8,graphs:7:3,23.486493113

real	0m23.491s
user	0m23.141s
sys	0m0.340s

Testing lattice under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/lattice.scm /tmp/larcenous/Bones/lattice.exe
bones /tmp/larcenous/Bones/lattice.scm > /tmp/larcenous/Bones/lattice.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/lattice.scm.s -o /tmp/larcenous/Bones/lattice.scm.o
gcc /tmp/larcenous/Bones/lattice.scm.o -o /tmp/larcenous/Bones/lattice.exe
Running...
Running lattice:44:10
Elapsed time: 25.935139091 seconds (26.0) for lattice:44:10
+!CSVLINE!+bones-8,lattice:44:10,25.935139091

real	0m25.939s
user	0m25.907s
sys	0m0.032s

Testing matrix under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/matrix.scm /tmp/larcenous/Bones/matrix.exe
bones /tmp/larcenous/Bones/matrix.scm > /tmp/larcenous/Bones/matrix.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/matrix.scm.s -o /tmp/larcenous/Bones/matrix.scm.o
gcc /tmp/larcenous/Bones/matrix.scm.o -o /tmp/larcenous/Bones/matrix.exe
Running...
Running matrix:5:5:2500
Elapsed time: 5.711974668 seconds (5.0) for matrix:5:5:2500
+!CSVLINE!+bones-8,matrix:5:5:2500,5.711974668

real	0m5.717s
user	0m5.692s
sys	0m0.020s

Testing maze under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/maze.scm /tmp/larcenous/Bones/maze.exe
bones /tmp/larcenous/Bones/maze.scm > /tmp/larcenous/Bones/maze.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/maze.scm.s -o /tmp/larcenous/Bones/maze.scm.o
gcc /tmp/larcenous/Bones/maze.scm.o -o /tmp/larcenous/Bones/maze.exe
Running...
Running maze:20:7:10000
Elapsed time: 4.42841536 seconds (5.0) for maze:20:7:10000
+!CSVLINE!+bones-8,maze:20:7:10000,4.42841536

real	0m4.437s
user	0m4.413s
sys	0m0.024s

Testing mazefun under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/mazefun.scm /tmp/larcenous/Bones/mazefun.exe
bones /tmp/larcenous/Bones/mazefun.scm > /tmp/larcenous/Bones/mazefun.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/mazefun.scm.s -o /tmp/larcenous/Bones/mazefun.scm.o
gcc /tmp/larcenous/Bones/mazefun.scm.o -o /tmp/larcenous/Bones/mazefun.exe
Running...
Running mazefun:11:11:10000
Elapsed time: 10.212448806 seconds (10.0) for mazefun:11:11:10000
+!CSVLINE!+bones-8,mazefun:11:11:10000,10.212448806

real	0m10.217s
user	0m10.197s
sys	0m0.020s

Testing nqueens under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/nqueens.scm /tmp/larcenous/Bones/nqueens.exe
bones /tmp/larcenous/Bones/nqueens.scm > /tmp/larcenous/Bones/nqueens.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/nqueens.scm.s -o /tmp/larcenous/Bones/nqueens.scm.o
gcc /tmp/larcenous/Bones/nqueens.scm.o -o /tmp/larcenous/Bones/nqueens.exe
Running...
Running nqueens:13:10
Elapsed time: 28.698171349 seconds (29.0) for nqueens:13:10
+!CSVLINE!+bones-8,nqueens:13:10,28.698171349

real	0m28.702s
user	0m28.678s
sys	0m0.024s

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

real	0m1.028s
user	0m1.004s
sys	0m0.024s
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
Elapsed time: 23.21836681 seconds (23.0) for parsing:2500
+!CSVLINE!+bones-8,parsing:2500,23.21836681

real	0m23.222s
user	0m23.166s
sys	0m0.056s

Testing peval under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/peval.scm /tmp/larcenous/Bones/peval.exe
bones /tmp/larcenous/Bones/peval.scm > /tmp/larcenous/Bones/peval.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/peval.scm.s -o /tmp/larcenous/Bones/peval.scm.o
gcc /tmp/larcenous/Bones/peval.scm.o -o /tmp/larcenous/Bones/peval.exe
Running...
Running peval:2000
Elapsed time: 4.480742113 seconds (4.0) for peval:2000
+!CSVLINE!+bones-8,peval:2000,4.480742113

real	0m4.485s
user	0m4.465s
sys	0m0.020s

Testing primes under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/primes.scm /tmp/larcenous/Bones/primes.exe
bones /tmp/larcenous/Bones/primes.scm > /tmp/larcenous/Bones/primes.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/primes.scm.s -o /tmp/larcenous/Bones/primes.scm.o
gcc /tmp/larcenous/Bones/primes.scm.o -o /tmp/larcenous/Bones/primes.exe
Running...
Running primes:1000:10000
Elapsed time: 3.166004656 seconds (3.0) for primes:1000:10000
+!CSVLINE!+bones-8,primes:1000:10000,3.166004656

real	0m3.171s
user	0m3.155s
sys	0m0.016s

Testing quicksort under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/quicksort.scm /tmp/larcenous/Bones/quicksort.exe
bones /tmp/larcenous/Bones/quicksort.scm > /tmp/larcenous/Bones/quicksort.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/quicksort.scm.s -o /tmp/larcenous/Bones/quicksort.scm.o
/tmp/larcenous/Bones/quicksort.scm.s:2693: error: symbol `___vector_2dmap' not defined
gcc /tmp/larcenous/Bones/quicksort.scm.o -o /tmp/larcenous/Bones/quicksort.exe
/sbin/ld: cannot find /tmp/larcenous/Bones/quicksort.scm.o: No such file or directory
collect2: error: ld returned 1 exit status
+!CSVLINE!+bones,quicksort,COMPILEERROR

Testing scheme under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/scheme.scm /tmp/larcenous/Bones/scheme.exe
bones /tmp/larcenous/Bones/scheme.scm > /tmp/larcenous/Bones/scheme.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/scheme.scm.s -o /tmp/larcenous/Bones/scheme.scm.o
/tmp/larcenous/Bones/scheme.scm.s:3609: error: symbol `___complex_3f' not defined
gcc /tmp/larcenous/Bones/scheme.scm.o -o /tmp/larcenous/Bones/scheme.exe
/sbin/ld: cannot find /tmp/larcenous/Bones/scheme.scm.o: No such file or directory
collect2: error: ld returned 1 exit status
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
Elapsed time: 44.831074882 seconds (45.0) for slatex:500
+!CSVLINE!+bones-8,slatex:500,44.831074882

real	0m44.835s
user	0m3.660s
sys	0m41.169s

Testing chudnovsky under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/chudnovsky.scm /tmp/larcenous/Bones/chudnovsky.exe
bones /tmp/larcenous/Bones/chudnovsky.scm > /tmp/larcenous/Bones/chudnovsky.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/chudnovsky.scm.s -o /tmp/larcenous/Bones/chudnovsky.scm.o
gcc /tmp/larcenous/Bones/chudnovsky.scm.o -o /tmp/larcenous/Bones/chudnovsky.exe
Running...
Running chudnovsky:50:500:50:1000
ERROR: returned incorrect result: (0 0 0 0 0 0 0 0 0 0)

real	0m0.067s
user	0m0.047s
sys	0m0.020s

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

real	0m4.783s
user	0m4.767s
sys	0m0.016s
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

real	0m31.913s
user	0m31.893s
sys	0m0.020s
+!CSVLINE!+bones,sboyer,CRASHED

Testing gcbench under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/gcbench.scm /tmp/larcenous/Bones/gcbench.exe
bones /tmp/larcenous/Bones/gcbench.scm > /tmp/larcenous/Bones/gcbench.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/gcbench.scm.s -o /tmp/larcenous/Bones/gcbench.scm.o
/tmp/larcenous/Bones/gcbench.scm.s:2827: error: symbol `___make_2dnode_2draw' not defined
/tmp/larcenous/Bones/gcbench.scm.s:2828: error: symbol `___left' not defined
/tmp/larcenous/Bones/gcbench.scm.s:2829: error: symbol `___right' not defined
/tmp/larcenous/Bones/gcbench.scm.s:2830: error: symbol `___i' not defined
/tmp/larcenous/Bones/gcbench.scm.s:2831: error: symbol `___j' not defined
/tmp/larcenous/Bones/gcbench.scm.s:2859: error: symbol `___left' not defined
/tmp/larcenous/Bones/gcbench.scm.s:2860: error: symbol `___node_2eleft' not defined
/tmp/larcenous/Bones/gcbench.scm.s:2861: error: symbol `___node_2eleft_2dset_21' not defined
/tmp/larcenous/Bones/gcbench.scm.s:2893: error: symbol `___right' not defined
/tmp/larcenous/Bones/gcbench.scm.s:2894: error: symbol `___node_2eright' not defined
/tmp/larcenous/Bones/gcbench.scm.s:2895: error: symbol `___node_2eright_2dset_21' not defined
/tmp/larcenous/Bones/gcbench.scm.s:2929: error: symbol `___i' not defined
/tmp/larcenous/Bones/gcbench.scm.s:2930: error: symbol `___node_2ei' not defined
/tmp/larcenous/Bones/gcbench.scm.s:2931: error: symbol `___node_2ei_2dset_21' not defined
/tmp/larcenous/Bones/gcbench.scm.s:2967: error: symbol `___j' not defined
/tmp/larcenous/Bones/gcbench.scm.s:2968: error: symbol `___node_2ej' not defined
/tmp/larcenous/Bones/gcbench.scm.s:2969: error: symbol `___node_2ej_2dset_21' not defined
/tmp/larcenous/Bones/gcbench.scm.s:2979: error: symbol `___class_4eode' not defined
/tmp/larcenous/Bones/gcbench.scm.s:2981: error: symbol `___class_4eode_3f' not defined
/tmp/larcenous/Bones/gcbench.scm.s:3003: error: symbol `___define_2drecord_2dtype' not defined
/tmp/larcenous/Bones/gcbench.scm.s:5475: error: symbol `___node_2eleft_2dset_21' not defined
/tmp/larcenous/Bones/gcbench.scm.s:5525: error: symbol `___node_2eright_2dset_21' not defined
/tmp/larcenous/Bones/gcbench.scm.s:5547: error: symbol `___node_2eleft' not defined
/tmp/larcenous/Bones/gcbench.scm.s:5600: error: symbol `___node_2eright' not defined
/tmp/larcenous/Bones/gcbench.scm.s:5630: error: symbol `___make_2dnode_2draw' not defined
/tmp/larcenous/Bones/gcbench.scm.s:5644: error: symbol `___make_2dnode_2draw' not defined
gcc /tmp/larcenous/Bones/gcbench.scm.o -o /tmp/larcenous/Bones/gcbench.exe
/sbin/ld: cannot find /tmp/larcenous/Bones/gcbench.scm.o: No such file or directory
collect2: error: ld returned 1 exit status
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

real	0m2.357s
user	0m2.337s
sys	0m0.020s
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
./bench: line 611:  9173 Killed                  "$1" < "$2"

real	4m59.997s
user	4m59.993s
sys	0m0.000s
+!CSVLINE!+bones,equal,ULIMITKILLED

Testing bv2string under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/bv2string.scm /tmp/larcenous/Bones/bv2string.exe
bones /tmp/larcenous/Bones/bv2string.scm > /tmp/larcenous/Bones/bv2string.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/bv2string.scm.s -o /tmp/larcenous/Bones/bv2string.scm.o
/tmp/larcenous/Bones/bv2string.scm.s:2814: error: symbol `___utf8_2d_3estring' not defined
/tmp/larcenous/Bones/bv2string.scm.s:2815: error: symbol `___string_2d_3eutf8' not defined
/tmp/larcenous/Bones/bv2string.scm.s:2958: error: symbol `___utf8_2d_3estring' not defined
/tmp/larcenous/Bones/bv2string.scm.s:2959: error: symbol `___string_2d_3eutf8' not defined
gcc /tmp/larcenous/Bones/bv2string.scm.o -o /tmp/larcenous/Bones/bv2string.exe
/sbin/ld: cannot find /tmp/larcenous/Bones/bv2string.scm.o: No such file or directory
collect2: error: ld returned 1 exit status
+!CSVLINE!+bones,bv2string,COMPILEERROR
