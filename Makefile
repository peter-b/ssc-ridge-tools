all:
	(cd libguile-tiffio && $(MAKE) all)

check: all tests

tests:
	@./runguile -c '(use-modules (unit-test)) (run-tests-on-command-line)' `find scm/unit-tests -name '*.scm'`