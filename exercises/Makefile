DIRS =  avl \
	bitonic \
	concurrency \
	derivative \
	dynamic \
	emulator \
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
	server \
	semantics \
	splay\
	streams\
	tracer\
	train\
	transport\
	twothree   

PONG = pong.ex ping.ex websocket.ex game.ex session.ex pong.html pong.js

WWW =  ./www

all:
	echo $(DIRS)
	for d in $(DIRS); do  echo ./$$d; cd $$d && make && cd ..; \
        done

install: pong assignments

assignments: 
	for d in $(DIRS); do \
	    mkdir -p $(WWW)/$$d;\
	    rsync -u  ./$$d/assignment.pdf $(WWW)/$$d/$$d.pdf;\
	done;

pong: 
	mkdir -p $(WWW)/pong;
	mkdir -p $(WWW)/pong/src;
	for f in $(PONG); do \
             rsync -uL ./pong/src/$$f  $(WWW)/pong/src/$$f;\
        done




clean : 
	for d in $(DIRS); do cd ./$$d && make clean && cd ..; \
        done

realclean : 
	for d in $(DIRS); do cd ./$$d && make realclean && cd ..; \
        done


