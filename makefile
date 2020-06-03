all: build

build:
		gsc-script -target js -exe -o index.html script.scm

publish:
		git add index.html
		git commit -m "Add new build of index.html"
		git push -u origin master
