DIRS =	huffman \
	derivative \
        introduction \
	interpreter \
	lambda \
	mandel \
	morse \
	operational \
	twothree



WWW =  ./www


all:
	echo $(DIRS)
	for d in $(DIRS); do  echo ./$$d; cd $$d && make && cd ..; \
        done

install: 
	for d in $(DIRS); do \
	    mkdir -p $(WWW)/$$d;\
	    rsync -u  ./$$d/assignment.pdf $(WWW)/$$d/$$d.pdf;\
	done


clean : 
	for d in $(DIRS); do cd ./$$d && make clean && cd ..; \
        done

realclean : 
	for d in $(DIRS); do cd ./$$d && make realclean && cd ..; \
        done


