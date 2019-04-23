NAME1 := lex
NAME2 := compare
NAME3 := spellCheck

PHONY: clean build
clean:
	@rm -f $(NAME) $<
	@echo "Build files removed!" $<
build:
	@echo "Compiling..." $<
	@gcc $(NAME1).c -g -Wall -Wextra -o $(NAME1).out $<
	@gcc $(NAME2).c -g -Wall -Wextra -o $(NAME2).out $<
	@gcc $(NAME3).c -g -Wall -Wextra -o $(NAME3).out $<
