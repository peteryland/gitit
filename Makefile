SHELL = /bin/bash

all: tmp-inst-ghc/usr/bin/gitit

tmp-inst-ghc/usr/bin/gitit:
	cabal new-configure -fplugins --prefix=/usr --bindir=/usr/bin --dynlibdir=/usr/lib --datadir=/usr/share/gitit --sysconfdir=/etc --libdir=/usr/lib/haskell-packages/ghc/lib --libexecdir=/usr/lib --builddir=dist-ghc --ghc-option=-optl-Wl\,-z\,relro\,-O2 --haddockdir=/usr/lib/ghc-doc/haddock/gitit-0.13.0.0/ --datasubdir=gitit --htmldir=/usr/share/doc/libghc-gitit-doc/html/ --global
	cabal new-build -fplugins --prefix=/usr --bindir=/usr/bin --dynlibdir=/usr/lib --datadir=/usr/share/gitit --sysconfdir=/etc --libdir=/usr/lib/haskell-packages/ghc/lib --libexecdir=/usr/lib --builddir=dist-ghc --ghc-option=-optl-Wl\,-z\,relro\,-O2 --haddockdir=/usr/lib/ghc-doc/haddock/gitit-0.13.0.0/ --datasubdir=gitit --htmldir=/usr/share/doc/libghc-gitit-doc/html/ --global
	rm -rf tmp-inst-ghc
	mkdir -p tmp-inst-ghc/usr/bin tmp-inst-ghc/usr/share/gitit/data/{static/{css,img/icons,fonts},templates}
	mkdir -p tmp-inst-ghc/var/lib/mgnl-doc/{static,templates,cache}
	mkdir -p tmp-inst-ghc/etc/apache2/sites-available
	cp dist-ghc/build/x86_64-linux/*/*/x/*/build/*/gitit dist-ghc/build/x86_64-linux/*/*/x/*/build/*/expireGititCache tmp-inst-ghc/usr/bin/
	patchelf --remove-needed libatomic.so.1 tmp-inst-ghc/usr/bin/gitit
	patchelf --remove-needed libutil.so.1 tmp-inst-ghc/usr/bin/gitit
	patchelf --remove-needed libatomic.so.1 tmp-inst-ghc/usr/bin/expireGititCache
	cp data/default.conf tmp-inst-ghc/usr/share/gitit/data/
	sed 's/^/# /' data/default.conf > tmp-inst-ghc/etc/gitit.conf
	cp data/templates/page.st tmp-inst-ghc/usr/share/gitit/data/templates/
	cp data/static/robots.txt tmp-inst-ghc/usr/share/gitit/data/static/
	cp data/static/css/{custom,print,micons,highlighting,ie}.css tmp-inst-ghc/usr/share/gitit/data/static/css/
	cp data/static/img/icons/{page,feed,folder}.png tmp-inst-ghc/usr/share/gitit/data/static/img/icons/
	cp data/static/img/icons/mgnl*.png tmp-inst-ghc/usr/share/gitit/data/static/img/icons/
	cp data/static/fonts/MagnoliaIcons.woff tmp-inst-ghc/usr/share/gitit/data/static/fonts
	mkdir -p tmp-inst-ghc/var/www
	cp -r debian/errors tmp-inst-ghc/var/www/
	cp debian/gitit.site.conf tmp-inst-ghc/etc/apache2/sites-available/mgnldoc.conf

install:
	cabal new-install --overwrite-policy=always --prefix=/usr --bindir=/usr/bin --dynlibdir=/usr/lib --datadir=/usr/share/gitit --sysconfdir=/etc --libdir=/usr/lib/haskell-packages/ghc/lib --libexecdir=/usr/lib --builddir=dist-ghc --ghc-option=-optl-Wl\,-z\,relro\,-O2 --haddockdir=/usr/lib/ghc-doc/haddock/gitit-0.13.0.0/ --datasubdir=gitit --htmldir=/usr/share/doc/libghc-gitit-doc/html/ --global

deb:
	rm -f ../gitit*.deb
	fakeroot debian/rules binary
	cd ..; \
	f=$$(echo gitit_*_amd64.deb); \
	scp $$f jack:/tmp/; \
	ssh jack sudo reprepro -C main -vb /var/www/debian-repo includedeb buster /tmp/$$f; \
	ssh jack rm -f /tmp/$$f

clean:
	@mkdir -p tmp-inst-ghc
	debian/rules clean
	rm -rf dist* tmp-inst-ghc

.PHONY: all clean install deb
