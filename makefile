tofumake: tofu.pl tofu_tests.pl
	swipl -l tofu_tests.pl -g run_tests -t halt
