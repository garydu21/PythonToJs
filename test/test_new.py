def double(n: int) -> int:
    #begin
    return(n + n)
    #end

def valeur_absolue(x: int) -> int:
    #begin
    res : int
    if x < 0:
        #begin
        res = 0 - x
        #end
    else:
        #begin
        res = x
        #end
    return(res)
    #end

def compte(n: int) -> int:
    #begin
    i : int
    i = 0
    while i < n:
        #begin
        i = i + 1
        #end
    return(i)
    #end

def afficher(x: int) -> int:
    #begin
    return(x)
    #end

res: int
ok: bool
res = double(5)
ok = res > 0
res = valeur_absolue(0 - 3)
res = compte(10)
afficher(res)