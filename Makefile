all:
	git submodule update --init
	wget http://sourceforge.net/projects/cedet/files/cedet/cedet-1.0.tar.gz/download
	mkdir plugins/cedet
	mv download plugins/cedet/cedet.tar.bz2
	cd plugins/cedet; tar -xvf cedet.tar.bz2
