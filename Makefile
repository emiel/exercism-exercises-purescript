clean:
	find $(CURDIR) -name .spago -type d -depth 3 -exec rm -rf {} \;
	find $(CURDIR) -name output -type d -depth 3 -exec rm -rf {} \;
