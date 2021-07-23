
****************************
Benchmarking Bones on Fri 16 Jul 07:11:23 UTC 2021 under Linux benchmarks 5.4.124-1-pve #1 SMP PVE 5.4.124-1 (Wed, 23 Jun 2021 13:47:09 +0200) x86_64 GNU/Linux

Testing browse under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/browse.scm /tmp/larcenous/Bones/browse.exe
bones /tmp/larcenous/Bones/browse.scm > /tmp/larcenous/Bones/browse.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/browse.scm.s -o /tmp/larcenous/Bones/browse.scm.o
gcc /tmp/larcenous/Bones/browse.scm.o -o /tmp/larcenous/Bones/browse.exe
Running...
Running browse:2000
Elapsed time: 0.56269106 seconds (1.0) for browse:2000
+!CSVLINE!+bones-8,browse:2000,0.56269106

real	0m0.569s
user	0m0.541s
sys	0m0.028s

Testing deriv under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/deriv.scm /tmp/larcenous/Bones/deriv.exe
bones /tmp/larcenous/Bones/deriv.scm > /tmp/larcenous/Bones/deriv.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/deriv.scm.s -o /tmp/larcenous/Bones/deriv.scm.o
gcc /tmp/larcenous/Bones/deriv.scm.o -o /tmp/larcenous/Bones/deriv.exe
Running...
Running deriv:10000000
Elapsed time: 3.379181229 seconds (4.0) for deriv:10000000
+!CSVLINE!+bones-8,deriv:10000000,3.379181229

real	0m3.383s
user	0m3.359s
sys	0m0.024s

Testing destruc under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/destruc.scm /tmp/larcenous/Bones/destruc.exe
bones /tmp/larcenous/Bones/destruc.scm > /tmp/larcenous/Bones/destruc.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/destruc.scm.s -o /tmp/larcenous/Bones/destruc.scm.o
gcc /tmp/larcenous/Bones/destruc.scm.o -o /tmp/larcenous/Bones/destruc.exe
Running...
Running destruc:600:50:4000
Elapsed time: 7.985357764 seconds (8.0) for destruc:600:50:4000
+!CSVLINE!+bones-8,destruc:600:50:4000,7.985357764

real	0m7.990s
user	0m7.974s
sys	0m0.016s

Testing diviter under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/diviter.scm /tmp/larcenous/Bones/diviter.exe
bones /tmp/larcenous/Bones/diviter.scm > /tmp/larcenous/Bones/diviter.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/diviter.scm.s -o /tmp/larcenous/Bones/diviter.scm.o
gcc /tmp/larcenous/Bones/diviter.scm.o -o /tmp/larcenous/Bones/diviter.exe
Running...
Running diviter:1000:1000000
Elapsed time: 2.037658077 seconds (2.0) for diviter:1000:1000000
+!CSVLINE!+bones-8,diviter:1000:1000000,2.037658077

real	0m2.041s
user	0m2.029s
sys	0m0.012s

Testing divrec under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/divrec.scm /tmp/larcenous/Bones/divrec.exe
bones /tmp/larcenous/Bones/divrec.scm > /tmp/larcenous/Bones/divrec.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/divrec.scm.s -o /tmp/larcenous/Bones/divrec.scm.o
gcc /tmp/larcenous/Bones/divrec.scm.o -o /tmp/larcenous/Bones/divrec.exe
Running...
Running divrec:1000:1000000
Elapsed time: 3.952513131 seconds (4.0) for divrec:1000:1000000
+!CSVLINE!+bones-8,divrec:1000:1000000,3.952513131

real	0m3.956s
user	0m3.932s
sys	0m0.024s

Testing puzzle under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/puzzle.scm /tmp/larcenous/Bones/puzzle.exe
bones /tmp/larcenous/Bones/puzzle.scm > /tmp/larcenous/Bones/puzzle.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/puzzle.scm.s -o /tmp/larcenous/Bones/puzzle.scm.o
gcc /tmp/larcenous/Bones/puzzle.scm.o -o /tmp/larcenous/Bones/puzzle.exe
Running...
Running puzzle:1000
Elapsed time: 13.102755462 seconds (13.0) for puzzle:1000
+!CSVLINE!+bones-8,puzzle:1000,13.102755462

real	0m13.106s
user	0m13.094s
sys	0m0.012s

Testing triangl under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/triangl.scm /tmp/larcenous/Bones/triangl.exe
bones /tmp/larcenous/Bones/triangl.scm > /tmp/larcenous/Bones/triangl.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/triangl.scm.s -o /tmp/larcenous/Bones/triangl.scm.o
gcc /tmp/larcenous/Bones/triangl.scm.o -o /tmp/larcenous/Bones/triangl.exe
Running...
Running triangl:22:1:50
Elapsed time: 9.559103565999999 seconds (9.0) for triangl:22:1:50
+!CSVLINE!+bones-8,triangl:22:1:50,9.559103565999999

real	0m9.563s
user	0m9.543s
sys	0m0.020s

Testing tak under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/tak.scm /tmp/larcenous/Bones/tak.exe
bones /tmp/larcenous/Bones/tak.scm > /tmp/larcenous/Bones/tak.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/tak.scm.s -o /tmp/larcenous/Bones/tak.scm.o
gcc /tmp/larcenous/Bones/tak.scm.o -o /tmp/larcenous/Bones/tak.exe
Running...
Running tak:40:20:11:1
Elapsed time: 10.290500403 seconds (11.0) for tak:40:20:11:1
+!CSVLINE!+bones-8,tak:40:20:11:1,10.290500403

real	0m10.294s
user	0m10.278s
sys	0m0.016s

Testing takl under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/takl.scm /tmp/larcenous/Bones/takl.exe
bones /tmp/larcenous/Bones/takl.scm > /tmp/larcenous/Bones/takl.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/takl.scm.s -o /tmp/larcenous/Bones/takl.scm.o
gcc /tmp/larcenous/Bones/takl.scm.o -o /tmp/larcenous/Bones/takl.exe
Running...
Running takl:40:20:12:1
Elapsed time: 6.572024349 seconds (6.0) for takl:40:20:12:1
+!CSVLINE!+bones-8,takl:40:20:12:1,6.572024349

real	0m6.576s
user	0m6.552s
sys	0m0.024s

Testing ntakl under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/ntakl.scm /tmp/larcenous/Bones/ntakl.exe
bones /tmp/larcenous/Bones/ntakl.scm > /tmp/larcenous/Bones/ntakl.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/ntakl.scm.s -o /tmp/larcenous/Bones/ntakl.scm.o
gcc /tmp/larcenous/Bones/ntakl.scm.o -o /tmp/larcenous/Bones/ntakl.exe
Running...
Running ntakl:40:20:12:1
Elapsed time: 6.712479413 seconds (7.0) for ntakl:40:20:12:1
+!CSVLINE!+bones-8,ntakl:40:20:12:1,6.712479413

real	0m6.716s
user	0m6.680s
sys	0m0.036s

Testing cpstak under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/cpstak.scm /tmp/larcenous/Bones/cpstak.exe
bones /tmp/larcenous/Bones/cpstak.scm > /tmp/larcenous/Bones/cpstak.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/cpstak.scm.s -o /tmp/larcenous/Bones/cpstak.scm.o
gcc /tmp/larcenous/Bones/cpstak.scm.o -o /tmp/larcenous/Bones/cpstak.exe
Running...
Running cpstak:40:20:11:1
Elapsed time: 13.482734141 seconds (14.0) for cpstak:40:20:11:1
+!CSVLINE!+bones-8,cpstak:40:20:11:1,13.482734141

real	0m13.486s
user	0m13.466s
sys	0m0.020s

Testing ctak under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/ctak.scm /tmp/larcenous/Bones/ctak.exe
bones /tmp/larcenous/Bones/ctak.scm > /tmp/larcenous/Bones/ctak.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/ctak.scm.s -o /tmp/larcenous/Bones/ctak.scm.o
gcc /tmp/larcenous/Bones/ctak.scm.o -o /tmp/larcenous/Bones/ctak.exe
Running...
Running ctak:32:16:8:1
Elapsed time: 1.576602789 seconds (1.0) for ctak:32:16:8:1
+!CSVLINE!+bones-8,ctak:32:16:8:1,1.576602789

real	0m1.580s
user	0m1.556s
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
Elapsed time: 24.389602235 seconds (25.0) for fib:40:5
+!CSVLINE!+bones-8,fib:40:5,24.389602235

real	0m24.393s
user	0m24.364s
sys	0m0.028s

Testing fibc under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/fibc.scm /tmp/larcenous/Bones/fibc.exe
bones /tmp/larcenous/Bones/fibc.scm > /tmp/larcenous/Bones/fibc.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/fibc.scm.s -o /tmp/larcenous/Bones/fibc.scm.o
gcc /tmp/larcenous/Bones/fibc.scm.o -o /tmp/larcenous/Bones/fibc.exe
Running...
Running fibc:30:10
Elapsed time: 1.823898734 seconds (2.0) for fibc:30:10
+!CSVLINE!+bones-8,fibc:30:10,1.823898734

real	0m1.827s
user	0m1.803s
sys	0m0.024s

Testing fibfp under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/fibfp.scm /tmp/larcenous/Bones/fibfp.exe
bones /tmp/larcenous/Bones/fibfp.scm > /tmp/larcenous/Bones/fibfp.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/fibfp.scm.s -o /tmp/larcenous/Bones/fibfp.scm.o
gcc /tmp/larcenous/Bones/fibfp.scm.o -o /tmp/larcenous/Bones/fibfp.exe
Running...
Running fibfp:35.0:10
Elapsed time: 5.297075197 seconds (5.0) for fibfp:35.0:10
+!CSVLINE!+bones-8,fibfp:35.0:10,5.297075197

real	0m5.301s
user	0m5.288s
sys	0m0.012s

Testing sum under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/sum.scm /tmp/larcenous/Bones/sum.exe
bones /tmp/larcenous/Bones/sum.scm > /tmp/larcenous/Bones/sum.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/sum.scm.s -o /tmp/larcenous/Bones/sum.scm.o
gcc /tmp/larcenous/Bones/sum.scm.o -o /tmp/larcenous/Bones/sum.exe
Running...
Running sum:10000:200000
Elapsed time: 32.979768634 seconds (33.0) for sum:10000:200000
+!CSVLINE!+bones-8,sum:10000:200000,32.979768634

real	0m32.983s
user	0m32.963s
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
Elapsed time: 10.414056964 seconds (11.0) for sumfp:1000000.0:500
+!CSVLINE!+bones-8,sumfp:1000000.0:500,10.414056964

real	0m10.418s
user	0m10.388s
sys	0m0.028s

Testing fft under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/fft.scm /tmp/larcenous/Bones/fft.exe
bones /tmp/larcenous/Bones/fft.scm > /tmp/larcenous/Bones/fft.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/fft.scm.s -o /tmp/larcenous/Bones/fft.scm.o
gcc /tmp/larcenous/Bones/fft.scm.o -o /tmp/larcenous/Bones/fft.exe
Running...
Running fft:65536:100
Elapsed time: 10.91267738 seconds (11.0) for fft:65536:100
+!CSVLINE!+bones-8,fft:65536:100,10.91267738

real	0m10.916s
user	0m10.888s
sys	0m0.028s

Testing mbrot under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/mbrot.scm /tmp/larcenous/Bones/mbrot.exe
bones /tmp/larcenous/Bones/mbrot.scm > /tmp/larcenous/Bones/mbrot.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/mbrot.scm.s -o /tmp/larcenous/Bones/mbrot.scm.o
gcc /tmp/larcenous/Bones/mbrot.scm.o -o /tmp/larcenous/Bones/mbrot.exe
Running...
Running mbrot:75:1000
Elapsed time: 15.825254341 seconds (16.0) for mbrot:75:1000
+!CSVLINE!+bones-8,mbrot:75:1000,15.825254341

real	0m15.829s
user	0m15.808s
sys	0m0.020s

Testing mbrotZ under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/mbrotZ.scm /tmp/larcenous/Bones/mbrotZ.exe
bones /tmp/larcenous/Bones/mbrotZ.scm > /tmp/larcenous/Bones/mbrotZ.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/mbrotZ.scm.s -o /tmp/larcenous/Bones/mbrotZ.scm.o
/tmp/larcenous/Bones/mbrotZ.scm.s:2338: error: symbol `____2d1_2e0_2d0_2e5i' not defined
/tmp/larcenous/Bones/mbrotZ.scm.s:2857: error: symbol `___make_2drectangular' not defined
/tmp/larcenous/Bones/mbrotZ.scm.s:3094: error: symbol `___real_2dpart' not defined
/tmp/larcenous/Bones/mbrotZ.scm.s:3124: error: symbol `___imag_2dpart' not defined
gcc /tmp/larcenous/Bones/mbrotZ.scm.o -o /tmp/larcenous/Bones/mbrotZ.exe
/usr/bin/ld: cannot find /tmp/larcenous/Bones/mbrotZ.scm.o: No such file or directory
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
Elapsed time: 6.611759982 seconds (7.0) for nucleic:50
+!CSVLINE!+bones-8,nucleic:50,6.611759982

real	0m6.615s
user	0m6.595s
sys	0m0.020s

Testing pi under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/pi.scm /tmp/larcenous/Bones/pi.exe
bones /tmp/larcenous/Bones/pi.scm > /tmp/larcenous/Bones/pi.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/pi.scm.s -o /tmp/larcenous/Bones/pi.scm.o
gcc /tmp/larcenous/Bones/pi.scm.o -o /tmp/larcenous/Bones/pi.exe
Running...
Running pi:50:500:50:100
./bench: line 608:  5681 Floating point exception(core dumped) "$1" < "$2"

real	0m0.008s
user	0m0.008s
sys	0m0.000s
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
Elapsed time: 13.39915515 seconds (14.0) for pnpoly:1000000
+!CSVLINE!+bones-8,pnpoly:1000000,13.39915515

real	0m13.403s
user	0m13.377s
sys	0m0.024s

Testing ray under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/ray.scm /tmp/larcenous/Bones/ray.exe
bones /tmp/larcenous/Bones/ray.scm > /tmp/larcenous/Bones/ray.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/ray.scm.s -o /tmp/larcenous/Bones/ray.scm.o
/tmp/larcenous/Bones/ray.scm.s:23556: error: invalid character in floating-point constant exponent: '.'
gcc /tmp/larcenous/Bones/ray.scm.o -o /tmp/larcenous/Bones/ray.exe
/usr/bin/ld: cannot find /tmp/larcenous/Bones/ray.scm.o: No such file or directory
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
Elapsed time: 9.809454549 seconds (10.0) for simplex:1000000
+!CSVLINE!+bones-8,simplex:1000000,9.809454549

real	0m9.814s
user	0m9.794s
sys	0m0.020s

Testing ack under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/ack.scm /tmp/larcenous/Bones/ack.exe
bones /tmp/larcenous/Bones/ack.scm > /tmp/larcenous/Bones/ack.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/ack.scm.s -o /tmp/larcenous/Bones/ack.scm.o
gcc /tmp/larcenous/Bones/ack.scm.o -o /tmp/larcenous/Bones/ack.exe
Running...
Running ack:3:12:2
Elapsed time: 33.147000819 seconds (33.0) for ack:3:12:2
+!CSVLINE!+bones-8,ack:3:12:2,33.147000819

real	0m33.152s
user	0m33.131s
sys	0m0.020s

Testing array1 under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/array1.scm /tmp/larcenous/Bones/array1.exe
bones /tmp/larcenous/Bones/array1.scm > /tmp/larcenous/Bones/array1.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/array1.scm.s -o /tmp/larcenous/Bones/array1.scm.o
gcc /tmp/larcenous/Bones/array1.scm.o -o /tmp/larcenous/Bones/array1.exe
Running...
Running array1:1000000:500
Elapsed time: 19.340384559 seconds (19.0) for array1:1000000:500
+!CSVLINE!+bones-8,array1:1000000:500,19.340384559

real	0m19.345s
user	0m19.320s
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
Elapsed time: 0.24673459 seconds (0.0) for string:500000:100
+!CSVLINE!+bones-8,string:500000:100,0.24673459

real	0m0.252s
user	0m0.224s
sys	0m0.028s

Testing sum1 under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/sum1.scm /tmp/larcenous/Bones/sum1.exe
bones /tmp/larcenous/Bones/sum1.scm > /tmp/larcenous/Bones/sum1.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/sum1.scm.s -o /tmp/larcenous/Bones/sum1.scm.o
gcc /tmp/larcenous/Bones/sum1.scm.o -o /tmp/larcenous/Bones/sum1.exe
Running...
Running sum1:25
Elapsed time: 44.433751722 seconds (44.0) for sum1:25
+!CSVLINE!+bones-8,sum1:25,44.433751722

real	0m44.439s
user	0m9.116s
sys	0m35.322s

Testing cat under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/cat.scm /tmp/larcenous/Bones/cat.exe
bones /tmp/larcenous/Bones/cat.scm > /tmp/larcenous/Bones/cat.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/cat.scm.s -o /tmp/larcenous/Bones/cat.scm.o
gcc /tmp/larcenous/Bones/cat.scm.o -o /tmp/larcenous/Bones/cat.exe
Running...
Running cat:50
./bench: line 608:  5808 Killed                  "$1" < "$2"

real	5m0.002s
user	0m12.064s
sys	4m47.925s
+!CSVLINE!+bones,cat,ULIMITKILLED

Testing tail under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/tail.scm /tmp/larcenous/Bones/tail.exe
bones /tmp/larcenous/Bones/tail.scm > /tmp/larcenous/Bones/tail.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/tail.scm.s -o /tmp/larcenous/Bones/tail.scm.o
/tmp/larcenous/Bones/tail.scm.s:2504: error: symbol `___read_2dline' not defined
gcc /tmp/larcenous/Bones/tail.scm.o -o /tmp/larcenous/Bones/tail.exe
/usr/bin/ld: cannot find /tmp/larcenous/Bones/tail.scm.o: No such file or directory
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
./bench: line 608:  5842 Killed                  "$1" < "$2"

real	5m0.004s
user	0m20.196s
sys	4m39.801s
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
Elapsed time: 101.448628271 seconds (101.0) for read1:2500
+!CSVLINE!+bones-8,read1:2500,101.448628271

real	1m41.453s
user	0m12.504s
sys	1m28.948s

Testing compiler under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/compiler.scm /tmp/larcenous/Bones/compiler.exe
bones /tmp/larcenous/Bones/compiler.scm > /tmp/larcenous/Bones/compiler.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/compiler.scm.s -o /tmp/larcenous/Bones/compiler.scm.o
/tmp/larcenous/Bones/compiler.scm.s:76318: error: symbol `___real_2dpart' not defined
/tmp/larcenous/Bones/compiler.scm.s:76352: error: symbol `___imag_2dpart' not defined
/tmp/larcenous/Bones/compiler.scm.s:76460: error: symbol `___numerator' not defined
/tmp/larcenous/Bones/compiler.scm.s:76494: error: symbol `___denominator' not defined
/tmp/larcenous/Bones/compiler.scm.s:107203: error: symbol `___real_2dpart' not defined
/tmp/larcenous/Bones/compiler.scm.s:107252: error: symbol `___imag_2dpart' not defined
/tmp/larcenous/Bones/compiler.scm.s:107298: error: symbol `___imag_2dpart' not defined
gcc /tmp/larcenous/Bones/compiler.scm.o -o /tmp/larcenous/Bones/compiler.exe
/usr/bin/ld: cannot find /tmp/larcenous/Bones/compiler.scm.o: No such file or directory
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
Elapsed time: 7.271483726 seconds (7.0) for conform:500
+!CSVLINE!+bones-8,conform:500,7.271483726

real	0m7.276s
user	0m7.252s
sys	0m0.024s

Testing dynamic under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/dynamic.scm /tmp/larcenous/Bones/dynamic.exe
bones /tmp/larcenous/Bones/dynamic.scm > /tmp/larcenous/Bones/dynamic.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/dynamic.scm.s -o /tmp/larcenous/Bones/dynamic.scm.o
gcc /tmp/larcenous/Bones/dynamic.scm.o -o /tmp/larcenous/Bones/dynamic.exe
Running...
Running dynamic:500
Elapsed time: 70.670585222 seconds (70.0) for dynamic:500
+!CSVLINE!+bones-8,dynamic:500,70.670585222

real	1m10.676s
user	0m17.768s
sys	0m52.906s

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

real	0m7.295s
user	0m7.266s
sys	0m0.028s
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
Elapsed time: 23.245174556 seconds (23.0) for graphs:7:3
+!CSVLINE!+bones-8,graphs:7:3,23.245174556

real	0m23.250s
user	0m23.225s
sys	0m0.024s

Testing lattice under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/lattice.scm /tmp/larcenous/Bones/lattice.exe
bones /tmp/larcenous/Bones/lattice.scm > /tmp/larcenous/Bones/lattice.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/lattice.scm.s -o /tmp/larcenous/Bones/lattice.scm.o
gcc /tmp/larcenous/Bones/lattice.scm.o -o /tmp/larcenous/Bones/lattice.exe
Running...
Running lattice:44:10
Elapsed time: 25.742157268 seconds (25.0) for lattice:44:10
+!CSVLINE!+bones-8,lattice:44:10,25.742157268

real	0m25.747s
user	0m25.730s
sys	0m0.016s

Testing matrix under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/matrix.scm /tmp/larcenous/Bones/matrix.exe
bones /tmp/larcenous/Bones/matrix.scm > /tmp/larcenous/Bones/matrix.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/matrix.scm.s -o /tmp/larcenous/Bones/matrix.scm.o
gcc /tmp/larcenous/Bones/matrix.scm.o -o /tmp/larcenous/Bones/matrix.exe
Running...
Running matrix:5:5:2500
Elapsed time: 5.696987918 seconds (6.0) for matrix:5:5:2500
+!CSVLINE!+bones-8,matrix:5:5:2500,5.696987918

real	0m5.702s
user	0m5.682s
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
Elapsed time: 4.498672228 seconds (4.0) for maze:20:7:10000
+!CSVLINE!+bones-8,maze:20:7:10000,4.498672228

real	0m4.508s
user	0m4.488s
sys	0m0.020s

Testing mazefun under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/mazefun.scm /tmp/larcenous/Bones/mazefun.exe
bones /tmp/larcenous/Bones/mazefun.scm > /tmp/larcenous/Bones/mazefun.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/mazefun.scm.s -o /tmp/larcenous/Bones/mazefun.scm.o
gcc /tmp/larcenous/Bones/mazefun.scm.o -o /tmp/larcenous/Bones/mazefun.exe
Running...
Running mazefun:11:11:10000
Elapsed time: 10.217149508 seconds (11.0) for mazefun:11:11:10000
+!CSVLINE!+bones-8,mazefun:11:11:10000,10.217149508

real	0m10.222s
user	0m10.206s
sys	0m0.016s

Testing nqueens under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/nqueens.scm /tmp/larcenous/Bones/nqueens.exe
bones /tmp/larcenous/Bones/nqueens.scm > /tmp/larcenous/Bones/nqueens.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/nqueens.scm.s -o /tmp/larcenous/Bones/nqueens.scm.o
gcc /tmp/larcenous/Bones/nqueens.scm.o -o /tmp/larcenous/Bones/nqueens.exe
Running...
Running nqueens:13:10
Elapsed time: 29.74741334 seconds (30.0) for nqueens:13:10
+!CSVLINE!+bones-8,nqueens:13:10,29.74741334

real	0m29.752s
user	0m29.727s
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

real	0m1.041s
user	0m1.025s
sys	0m0.016s
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
Elapsed time: 23.413145227 seconds (23.0) for parsing:2500
+!CSVLINE!+bones-8,parsing:2500,23.413145227

real	0m23.418s
user	0m23.357s
sys	0m0.060s

Testing peval under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/peval.scm /tmp/larcenous/Bones/peval.exe
bones /tmp/larcenous/Bones/peval.scm > /tmp/larcenous/Bones/peval.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/peval.scm.s -o /tmp/larcenous/Bones/peval.scm.o
gcc /tmp/larcenous/Bones/peval.scm.o -o /tmp/larcenous/Bones/peval.exe
Running...
Running peval:2000
Elapsed time: 4.428379587 seconds (4.0) for peval:2000
+!CSVLINE!+bones-8,peval:2000,4.428379587

real	0m4.433s
user	0m4.409s
sys	0m0.024s

Testing primes under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/primes.scm /tmp/larcenous/Bones/primes.exe
bones /tmp/larcenous/Bones/primes.scm > /tmp/larcenous/Bones/primes.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/primes.scm.s -o /tmp/larcenous/Bones/primes.scm.o
gcc /tmp/larcenous/Bones/primes.scm.o -o /tmp/larcenous/Bones/primes.exe
Running...
Running primes:1000:10000
Elapsed time: 3.19684377 seconds (3.0) for primes:1000:10000
+!CSVLINE!+bones-8,primes:1000:10000,3.19684377

real	0m3.202s
user	0m3.186s
sys	0m0.016s

Testing quicksort under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/quicksort.scm /tmp/larcenous/Bones/quicksort.exe
bones /tmp/larcenous/Bones/quicksort.scm > /tmp/larcenous/Bones/quicksort.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/quicksort.scm.s -o /tmp/larcenous/Bones/quicksort.scm.o
/tmp/larcenous/Bones/quicksort.scm.s:2745: error: symbol `___vector_2dmap' not defined
gcc /tmp/larcenous/Bones/quicksort.scm.o -o /tmp/larcenous/Bones/quicksort.exe
/usr/bin/ld: cannot find /tmp/larcenous/Bones/quicksort.scm.o: No such file or directory
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
/usr/bin/ld: cannot find /tmp/larcenous/Bones/scheme.scm.o: No such file or directory
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
Elapsed time: 54.915512165 seconds (55.0) for slatex:500
+!CSVLINE!+bones-8,slatex:500,54.915512165

real	0m54.920s
user	0m3.851s
sys	0m43.542s

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
user	0m0.044s
sys	0m0.024s

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

real	0m4.859s
user	0m4.838s
sys	0m0.020s
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

real	0m32.069s
user	0m32.048s
sys	0m0.020s
+!CSVLINE!+bones,sboyer,CRASHED

Testing gcbench under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/gcbench.scm /tmp/larcenous/Bones/gcbench.exe
bones /tmp/larcenous/Bones/gcbench.scm > /tmp/larcenous/Bones/gcbench.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/gcbench.scm.s -o /tmp/larcenous/Bones/gcbench.scm.o
/tmp/larcenous/Bones/gcbench.scm.s:2879: error: symbol `___make_2dnode_2draw' not defined
/tmp/larcenous/Bones/gcbench.scm.s:2880: error: symbol `___left' not defined
/tmp/larcenous/Bones/gcbench.scm.s:2881: error: symbol `___right' not defined
/tmp/larcenous/Bones/gcbench.scm.s:2882: error: symbol `___i' not defined
/tmp/larcenous/Bones/gcbench.scm.s:2883: error: symbol `___j' not defined
/tmp/larcenous/Bones/gcbench.scm.s:2911: error: symbol `___left' not defined
/tmp/larcenous/Bones/gcbench.scm.s:2912: error: symbol `___node_2eleft' not defined
/tmp/larcenous/Bones/gcbench.scm.s:2913: error: symbol `___node_2eleft_2dset_21' not defined
/tmp/larcenous/Bones/gcbench.scm.s:2945: error: symbol `___right' not defined
/tmp/larcenous/Bones/gcbench.scm.s:2946: error: symbol `___node_2eright' not defined
/tmp/larcenous/Bones/gcbench.scm.s:2947: error: symbol `___node_2eright_2dset_21' not defined
/tmp/larcenous/Bones/gcbench.scm.s:2981: error: symbol `___i' not defined
/tmp/larcenous/Bones/gcbench.scm.s:2982: error: symbol `___node_2ei' not defined
/tmp/larcenous/Bones/gcbench.scm.s:2983: error: symbol `___node_2ei_2dset_21' not defined
/tmp/larcenous/Bones/gcbench.scm.s:3019: error: symbol `___j' not defined
/tmp/larcenous/Bones/gcbench.scm.s:3020: error: symbol `___node_2ej' not defined
/tmp/larcenous/Bones/gcbench.scm.s:3021: error: symbol `___node_2ej_2dset_21' not defined
/tmp/larcenous/Bones/gcbench.scm.s:3031: error: symbol `___class_4eode' not defined
/tmp/larcenous/Bones/gcbench.scm.s:3033: error: symbol `___class_4eode_3f' not defined
/tmp/larcenous/Bones/gcbench.scm.s:3055: error: symbol `___define_2drecord_2dtype' not defined
/tmp/larcenous/Bones/gcbench.scm.s:3157: error: symbol `___main' not defined
/tmp/larcenous/Bones/gcbench.scm.s:5527: error: symbol `___node_2eleft_2dset_21' not defined
/tmp/larcenous/Bones/gcbench.scm.s:5577: error: symbol `___node_2eright_2dset_21' not defined
/tmp/larcenous/Bones/gcbench.scm.s:5599: error: symbol `___node_2eleft' not defined
/tmp/larcenous/Bones/gcbench.scm.s:5652: error: symbol `___node_2eright' not defined
/tmp/larcenous/Bones/gcbench.scm.s:5682: error: symbol `___make_2dnode_2draw' not defined
/tmp/larcenous/Bones/gcbench.scm.s:5696: error: symbol `___make_2dnode_2draw' not defined
gcc /tmp/larcenous/Bones/gcbench.scm.o -o /tmp/larcenous/Bones/gcbench.exe
/usr/bin/ld: cannot find /tmp/larcenous/Bones/gcbench.scm.o: No such file or directory
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

real	0m2.363s
user	0m2.339s
sys	0m0.024s
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
./bench: line 608:  6155 Killed                  "$1" < "$2"

real	4m59.997s
user	4m59.988s
sys	0m0.004s
+!CSVLINE!+bones,equal,ULIMITKILLED

Testing bv2string under Bones
Including prelude /home/nex/src/r7rs-benchmarks/src/Bones-prelude.scm
Compiling...
bones_comp /tmp/larcenous/Bones/bv2string.scm /tmp/larcenous/Bones/bv2string.exe
bones /tmp/larcenous/Bones/bv2string.scm > /tmp/larcenous/Bones/bv2string.scm.s
nasm -I/usr/share/bones -f elf64 /tmp/larcenous/Bones/bv2string.scm.s -o /tmp/larcenous/Bones/bv2string.scm.o
/tmp/larcenous/Bones/bv2string.scm.s:2866: error: symbol `___utf8_2d_3estring' not defined
/tmp/larcenous/Bones/bv2string.scm.s:2867: error: symbol `___string_2d_3eutf8' not defined
/tmp/larcenous/Bones/bv2string.scm.s:3010: error: symbol `___utf8_2d_3estring' not defined
/tmp/larcenous/Bones/bv2string.scm.s:3011: error: symbol `___string_2d_3eutf8' not defined
gcc /tmp/larcenous/Bones/bv2string.scm.o -o /tmp/larcenous/Bones/bv2string.exe
/usr/bin/ld: cannot find /tmp/larcenous/Bones/bv2string.scm.o: No such file or directory
collect2: error: ld returned 1 exit status
+!CSVLINE!+bones,bv2string,COMPILEERROR
