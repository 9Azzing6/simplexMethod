import matrixLib

var dataF = @[4.0, -1.0, -1.0, 1.0, 1.0, 0.0, 7, -1.0, 2.0, 1.0, 0.0, 1.0, 7.0, 2.0, -1.0, 0.0, 1.0, 1.0, 5.0, -3.0, 1.0, 0.0, 0.0, 0.0]

# var data = @[2.0, 1.0, -2.0, 2.0, -2.0, 1.0, 5.0, 1.0, 1.0, 5.0, -3.0, 1.0]
var mat: matrix = raspMatrix(4, 6, dataF)
# var mat2: matrix = mat

proc fRowCheck(m: matrix): tuple[check: bool, j_ind: int] = #проверка опорного плана на оптимальность и нахождения индекса отрицательного элемента в f-строке, если таковой имеется
    result.check = false
    result.j_ind = 1

    var max = 0.0

    for j in 0 .. m.getColumns() - 1:
        if m[m.getRows() - 1, j]  < max:
            max = m[m.getRows() - 1, j]
            result.check = true
            result.j_ind = j

proc inf_funcCheck(m: matrix, j_ind: int): bool = #проверка функции на неограниченность
    result = true
    for i in 0 .. m.getRows() - 1:
        if (m[i, j_ind] > 0 and m[i, 0] > 0) or (m[i, j_ind] < 0 and m[i, 0] < 0):
            result = false
            break

proc indexSO(m: matrix, j_ind: int): int = #рассчет СО и выбор разрешающей строки
    result = -1
    var 
        so: float = 0.0
        min: float = Inf
    for i in 0 .. m.getRows() - 1:
        if ((m[i, 0] > 0 and m[i, j_ind] > 0) or (m[i, 0] < 0 and m[i, j_ind] < 0)) and m[i, j_ind] != 0:
            so = m[i, 0] / m[i, j_ind]
            if so < min:
                min = so
                result = i

proc jordan_sq(m: matrix, m2: var matrix, i_ind: int, j_ind: int): matrix = #правило прямоугольника
    for i in 0 .. m.getRows() - 1:
        if i == i_ind: continue
        for j in 0 .. m.getColumns() - 1:
            if j == j_ind: continue
            m2[i, j]= (m[i_ind, j_ind] * m[i, j] - m[i_ind, j] * m[i, j_ind]) / m[i_ind, j_ind]
        result = m2

proc jordan(m: matrix, i_ind: int, j_ind: int): matrix = #жордановы преобразования разрешающей строки и разрешающего столбца
    var m2: matrix = raspMatrix(m.getRows(), m.getColumns()) 
    m2[i_ind, j_ind]= 1 / m[i_ind, j_ind] #замена разрешающего элемента на обратный
    for j in 0 .. m.getColumns() - 1:
        if j == j_ind: continue
        m2[i_ind, j]= m[i_ind, j] / m[i_ind, j_ind]
    for i in 0 .. m.getRows() - 1:
        if i == i_ind: continue
        m2[i, j_ind]= m[i, j_ind] * (-1) / m[i_ind, j_ind]
    result = jordan_sq(m, m2, i_ind, j_ind)






proc swapX(hor_x, vert_x: var seq[string], i_ind, j_ind: int): void=
    var buffer: string = hor_x[j_ind - 1]
    hor_x[j_ind - 1] = vert_x[i_ind]
    vert_x[i_ind] = buffer 
    
    
proc desiredLength(str: string, des_len: int): int =
    result = des_len - str.len()

proc simplexTableToString(m: matrix, i_ind: int, j_ind: int, hor_x, vert_x: seq[string]): string =
    var 
        shapka: seq[string] = @["1"]
        desired_lenght: int = 8
    # делаем шапочку

    result = "БП"
    for iter in 0 .. int(desiredLength("БП", desired_lenght) / 2) - 1:
        result.add(" ")
    result.add("|")
    for i_sh in 0 .. len(shapka) - 1:
        for iter in 0 .. desiredLength(shapka[i_sh], desired_lenght) - 1:
            if iter == int(desiredLength(shapka[i_sh], desired_lenght) / 2):
                result.add(shapka[i_sh])
            else: result.add(" ")
        result.add(" |")

    for x in 0 .. len(hor_x) - 1:
        for iter in 0 .. desiredLength(hor_x[x], desired_lenght) - 1:
            if iter == int(desiredLength("-"&hor_x[x], desired_lenght) / 2):
                result.add("-"&hor_x[x])
            else: result.add(" ")
        result.add(" |")


    result.add("\n")  

    for i in 0 .. m.getRows() - 1:
        if i < len(vert_x):
            result.add(vert_x[i] & " = |")
        else: result.add("f =  |")
        for  j in 0 .. m.getColumns() - 1:
            for iter in 0 .. desiredLength($m[i,j], desired_lenght) - 1:
                if iter == int(desiredLength($m[i,j], desired_lenght) / 2):
                    result.add($m[i,j])
                    if (i == i_ind) and (j == j_ind): result.add("*")
                else: result.add(" ")
            if j != m.getColumns() - 1:
                result.add("  |  ")
        result.add("  |")
        if i != m.getRows() - 1:
            result.add("\n")    

proc checkOP(vert_x: seq[string]): tuple[check: bool, i_indeces_zero: seq[int]] =
    result.check = false
    for zero in 0 .. len(vert_x) - 1:
        if vert_x[zero] == "0":
            result.check = true
            result.i_indeces_zero.add(zero)
    if result.check:
        echo "опорный план отсуствует"
    else: echo "опорный план присуттсвует"

proc terminateNullColumn(m: matrix, hor_x: var seq[string]): matrix =
    result = raspMatrix(m.getRows(), m.getColumns() - 1)
    var j_ind: int
    for x in 0 .. len(hor_x) - 1:
        if hor_x[x] == "0":
            j_ind = x + 1
            break
    hor_x.delete(j_ind - 1)


    for i in 0 .. result.getRows() - 1:
        for j in 0 .. result.getColumns() - 1:
            if j == j_ind: continue
            else: continue
                
    echo "hello from terminator 2: "
    echo result.matrixToString()
    echo "hello from terminator 3: "
    echo m.matrixToString()

proc findOP(m: var matrix, hor_x, vert_x: var seq[string]): matrix =
    var iteration: int = 0
    var j_ind: int
    echo "поиск опорного плана"
    while checkOP(vert_x).check:
        echo "поиск"
        echo "iteration: ", iteration
        for j in 1 .. m.getColumns() - 1:
            if (indexSO(m, j) > 0) and (indexSO(m, j) in checkOP(vert_x).i_indeces_zero):
                j_ind = j
                break
        echo simplexTableToString(m, indexSO(m, j_ind), j_ind, hor_x, vert_x)
        swapX(hor_x, vert_x, indexSO(m, j_ind), j_ind)
        m = jordan(m, indexSO(m, j_ind), j_ind)
        m = terminateNullColumn(m, hor_x)
        iteration += 1
    echo "был получен опорный план"
    echo "табличка, содержащая опорный план:"
    echo simplexTableToString(m, -1, -1, hor_x, vert_x)
    

    result = m

proc firstTry(m: var matrix, hor_x, vert_x: var seq[string]): void =
    var
        hor_x: seq[string] = @["x1", "x2"]
        vert_x: seq[string] = @["x4", "x3", "x5"]
    var iteration: int = 0

    while fRowCheck(m).check:
        
        echo "iteration: ", iteration
        echo simplexTableToString(m, indexSO(m, fRowCheck(m).j_ind), fRowCheck(m).j_ind, hor_x, vert_x)
        if inf_funcCheck(m, fRowCheck(m).j_ind):
            echo "задача не имеет решения, f -> inf"
            break
        swapX(hor_x, vert_x, indexSO(m, fRowCheck(m).j_ind), fRowCheck(m).j_ind)
        
        m = jordan(m, indexSO(m, fRowCheck(m).j_ind), fRowCheck(m).j_ind)
        iteration += 1
    echo "был получен оптимальный план"
    echo "финальная табличка: "
    echo simplexTableToString(m, -1, -1, hor_x, vert_x)



# echo "*******************************"

# firstTry(mat, hor_x, vert_x)
var
        hor_x: seq[string] = @["x1", "x2", "x3", "x4", "x5"]
        vert_x: seq[string] = @["0", "0", "0"]
echo findOP(mat, hor_x, vert_x).matrixToString