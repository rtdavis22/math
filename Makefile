compile:
	sbt compile

test:
	sbt test

lint:
	sbt scalastyle test:scalastyle
