DIRS =  avl \
	bitonic \
	carlo \
	concurrency \
	derivative \
	dynamic \
	emulator \
	environment \
	expression \
	hanoi \
	higher \
	huffman \
	interpreter \
	introduction \
	lambda \
	lzw  \
	mandel \
	mips \
	morse \
	mutex \
	operational \
	philosophers \
	primes \
	rick \
	server \
	semantics \
	splay\
	streams\
	tracer\
	train\
	transport\
	twothree   

ADVENT22 = day16.ex day16.csv

PONG = pong.ex ping.ex websocket.ex game.ex session.ex pong.html pong.js

REPORT = report.pdf report.tar.gz

WWW =  ./www

all:
	echo $(DIRS)
	for d in $(DIRS); do  echo ./$$d; cd $$d && make && cd ..; \
        done
	cd report && make && cd ..;

install: 
	for d in $(DIRS); do \
	    mkdir -p $(WWW)/$$d;\
	    rsync -u  ./$$d/assignment.pdf $(WWW)/$$d/$$d.pdf;\
	done;
	mkdir -p $(WWW)/pong;
	mkdir -p $(WWW)/pong/src;
	for f in $(PONG); do \
             rsync -uL ./pong/src/$$f  $(WWW)/pong/src/$$f;\
        done
	mkdir -p $(WWW)/advent/2022
	for f in $(ADVENT22); do\
             rsync -uL ./advent/2022/src/$$f $(WWW)/advent/2022/$$f;\
	done
	mkdir -p $(WWW)/report
	for f in $(REPORT); do \
             rsync -uL ./report/$$f  $(WWW)/report/$$f;\
        done	


clean: 
	for d in $(DIRS); do cd ./$$d && make clean && cd ..; \
        done

realclean: clean
	for d in $(DIRS); do cd ./$$d && make realclean && cd ..; \
        done


