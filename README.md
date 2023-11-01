# Nepali programming language

## 1) String length counting using only loop
```code
rakha name ma 'kagati foundation';
rakha str_len ma 0;
ghumau name patak |char| suru
    yadi char barabar ' ' chhaina suru 
        rakha str_len ma str_len + 1;
    antya
antya

dekhau str_len;
```

## 2) String length counting using loop and function
```code
karya strlen(str) suru
    rakha len ma 0;
    ghumau str patak suru
        rakha len ma len + 1;
    antya
    farkau len;
antya

dekhau strlen("kagati foundation");
```