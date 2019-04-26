NAME1 := lex
NAME2 := compare
NAME3 := spellCheck

PHONY: clean build
clean:
	@rm -f $(NAME1).out $(NAME2).out $(NAME3).out $<
	@rm -f lab4.tar $<
	@echo "Build files removed!" $<
build:
	@echo "Compiling..." $<
	@gcc $(NAME1).c -g -Wall -Wextra -o $(NAME1).out $<
	@gcc $(NAME2).c -g -Wall -Wextra -o $(NAME2).out $<
	@gcc $(NAME3).c -g -Wall -Wextra -o $(NAME3).out $<
	@tar -cvf lab4.tar $(NAME1).c $(NAME2).c $(NAME3).c words.txt $<
