PDFLATEX = pdflatex

DIRS = 	asynchronous \
	complexity \
	concurrency \
	derivative\
	dns \
	dynamic \
	evaluation \
	higher \
	interpreter \
	introduction \
	lambda \
	overview \
	parallel \
	pong \
	recursion \
	tracer \
	transport \
	trees \
	types 


WWW =  ./www

PONG = pong.ex websocket.ex game.ex session.ex pong.html pong.js

all:
	echo $(DIRS)
	for d in $(DIRS); do  echo ./$$d; cd $$d && make && cd ..; \
        done
link:
	for d in $(DIRS); do\
             cd $$d;\
             ln -s ../include/Makefile .; \
	     ln -s ../include/handout.tex .; \
             cd ../;\
        done

install: 
	for d in $(DIRS); do \
             rsync -u ./$$d/handout-nup.pdf $(WWW)/$$d.pdf;\
        done



ipong : 
	for f in $(PONG); do \
             rsync -uL ./pong/src/$$f  $(WWW)/pong/$$f;\
        done



clean : 
	for d in $(DIRS); do cd ./$$d && make clean && cd ..; \
        done

realclean : 
	for d in $(DIRS); do cd ./$$d && make realclean && cd ..; \
        done


