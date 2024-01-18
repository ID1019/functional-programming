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
	operational \
	parallel \
	pong \
	recursion \
	streams \
	structures \
	tracer \
	transport \
	trees \
	types 


WWW =  ./www


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





clean : 
	for d in $(DIRS); do cd ./$$d && make clean && cd ..; \
        done

realclean : 
	for d in $(DIRS); do cd ./$$d && make realclean && cd ..; \
        done


