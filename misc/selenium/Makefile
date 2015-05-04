# This Makefile may help you figure out how to run the selenium grid
# service required for the webdriver frontend tests to work.  It is in
# no way well-written or stable, and you should only run it after you
# have read it.
#
# Some more pointers:
#
# Download the standalone grid server from here:
#
#     http://selenium-release.storage.googleapis.com/index.html
#
# Run like this:
#
# $ java -jar selenium-server-standalone-2.45.0.jar -role hub
# $ java -jar selenium-server-standalone-2.45.0.jar -role node -hub http://localhost:4444/grid/register
#
# (You may have to change the version number to what you downloaded.)
#
# Here is a slightly outdated tutorial with partially helpful further
# instructions: https://code.google.com/p/selenium/wiki/Grid2.


SELENIUM_HOST=localhost

SELENIUM_HUB_PORT=4451
# (default: 4444)

SELENIUM_NODE_PORT=4452
# (default: 5555)

SELENIUM_ARGS=-jar selenium-server-standalone-2.45.0.jar -host $(SELENIUM_HOST)

default:
	cat Makefile
	make kill start watch

init:
	curl http://selenium-release.storage.googleapis.com/2.45/selenium-server-standalone-2.45.0.jar > selenium-server-standalone-2.45.0.jar
	mkdir -p log

kill:
	killall java > /dev/null 2>&1 || true
	killall tail > /dev/null 2>&1 || true

start:
	nohup java $(SELENIUM_ARGS) -port $(SELENIUM_HUB_PORT) -role hub > log/hub 2>&1 &
	sleep 3
	nohup java $(SELENIUM_ARGS) -port $(SELENIUM_NODE_PORT) -role node -hub http://$(SELENIUM_HOST):$(SELENIUM_HUB_PORT)/grid/register > log/node 2>&1 &

watch:
	( sleep 1; tail -f log/hub ) &
	( sleep 1; tail -f log/node ) &
	@echo "\n\npress ^D to shut down selenium.\n\n\n"
	cat > /dev/null
	make kill

# (on debian: apt-get install xvfb)
# after you run this, set DISPLAY=:1 and restart selenium.
xvfb:
	killall Xvfb || true
	sleep 0.3
	killall -9 Xvfb || true
	sleep 0.2
	nohup Xvfb :1 > log/Xvfb 2>&1 &
