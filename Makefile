compile:
	sbt compile

test:
	sbt test

lint:
	sbt scalastyle

lint-tests:
	sbt test:scalastyle
