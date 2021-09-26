.PHONY: all clean

all:
	$(MAKE) -C outputs

clean:
	rm -f outputs/*.pdf outputs/*.txt
