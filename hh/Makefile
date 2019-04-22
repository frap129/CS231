NAME := hh

PHONY: clean build
clean:
	@rm -f $(NAME) $<
	@echo "Build files removed!" $<
build:
	@echo "Compiling..." $<
	@ghc $(NAME).hs $<
	@echo "Done! Final Binary: ./$(NAME)" $<

