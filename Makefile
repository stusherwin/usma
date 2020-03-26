install:
	stack build --fast --copy-bins
	stack exec usma-upgradedb 0