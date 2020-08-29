## Chapter 5

### Known Issues:

  - ```adjoin-arg``` is not implemented properly for the ec-evaluators.  It
    should use append ([footnote 22][fn-22]), but is instead implemented with
    ```cons``` throughout this directory.  This means that all the argument
    lists are in the reverse of the expected order.

### Unknown Issues:

  - ``` ... ```
  - ``` ... ```
  - ``` ... ```



[fn-22]:https://www.sicp-book.com/book-Z-H-34.html#footnote_Temp_771