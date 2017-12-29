SSH_HOST=fontobene.dbrgn.ch
SSH_PORT=22
SSH_USER=danilo
SSH_TARGET_DIR=/srv/www/dbrgn/fontobene/pathvis/

help:
	@echo "Use one of the following commands: setup, test, run, clean, dist, deploy"

setup:
	npm install
	elm-make

test:
	npm test

run:
	npm run dev

clean:
	rm -rf dist/

dist: clean
	npm run build

deploy: dist
	echo "Deploying"
	rsync -e "ssh -p $(SSH_PORT)" -P -p --chmod=ug=rwX,o=rX --group=www-data -rvzc --delete dist/ $(SSH_USER)@$(SSH_HOST):$(SSH_TARGET_DIR) --cvs-exclude

.PHONY: setup test run clean dist deploy
