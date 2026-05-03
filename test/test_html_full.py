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

x: int
x = int(input("Enter value x: "))
print("Result: ", valeur_absolue(x))