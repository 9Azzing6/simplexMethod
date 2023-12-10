#кляс матрыц, аб'ект захоўвае размернасці матрыц і плоскі лічбовы масіў (усе лічбы матрыц е лічбы с кропкамі і захоўваюцца у аднамерным масіве) 
type 
    matrix* = object
        sheragi*: int
        cols*: int
        data*: seq[float]

#гетеры размернасцей матрыцы, шэрагі і калонкі
proc getRows*(m: matrix): int {.inline.} = m.sheragi
proc getColumns*(m: matrix): int {.inline.} = m.cols

#метад запаўнення плоскага лічбовага масіва па дэфолту нулямі
proc dataToZero(sheragi, cols: int): seq[float] = 
    for i in 0 .. cols * sheragi - 1:
        result.add(0.0)

#канструктар кляса matrix, на уваход ідуць шэрагі, калонкі і лічбовы плоскі масіў seq[float] (па дэфолту масіў запалняецца нулямі)
proc raspMatrix*(sheragi, cols: int, providedData: seq[float] = dataToZero(sheragi, cols)): matrix = 
    if cols * sheragi != providedData.len():
        raise newException(IndexDefect, "размернасьці матрыцы не супадаюць з колькасцю эляментаў providedData")
    result.cols = cols
    result.sheragi = sheragi
    newSeq(result.data, cols * sheragi)
    for i in 0 .. cols * sheragi - 1:
        result.data[i] = providedData[i]

#дапаможны метад для доступу па індэксу да эляментаў матрыц
proc index(matrix: matrix, cols, sheragi: int): int = 
    result = sheragi * matrix.getColumns() + cols

#асноўны метад для доступу па індэксу да эляментаў матрыцы
proc `[]`*(matrix: matrix, sheragi, cols: int): float =
    if cols < 0 or cols > (matrix.getColumns() - 1):
        raise newException(IndexDefect, "індэкс не ўваходзіць у размернасць матрыцы: калонкі")
    if sheragi < 0 or sheragi > (matrix.getRows() - 1):
        raise newException(IndexDefect, "індэкс не ўваходзіць у размернасць матрыцы: шэрагі")
    result = matrix.data[matrix.index(cols, sheragi)]

#дапаможны метад для складання і аднімання матрыц, праверае ці падыходзяць для складання дадзеные матрыцы праз іхнія размернасці
proc areAllDimsEqual*(matrix1, matrix2: matrix): bool =
    if matrix1.getColumns() == matrix2.getColumns() and matrix1.getRows() == matrix2.getRows():
        result = true
    else: result = false

#сэтар эляментаў матрыцы па індэксам шэрагаў і калонак; патрэбна для транспаніравання матрыц
proc `[]=`*(matrix: var matrix, sheragi, cols: int, element: float): void = 
    matrix.data[matrix.index(cols, sheragi)] = element

#асноўны метад складання матрыц, выкарыстоўвае дапаможны метад areAllDimsEqual(m1, m2: matrix)
proc `+`*(matrix1, matrix2: matrix): matrix =
    if areAllDimsEqual(matrix1, matrix2) == false:
        raise newException(IndexDefect, "матрыцы немагчыма складаць(+): не супадаюць размернасці")
    result.cols = matrix1.getColumns()
    result.sheragi = matrix1.getRows()
    for i in 0 .. matrix1.getRows() * matrix1.getColumns() - 1:
        result.data.add(matrix1.data[i] + matrix2.data[i])

#асноўны метад аднімання матрыц, выкарыстоўвае дапаможны метад areAllDimsEqual(m1, m2: matrix): bool
proc `-`*(matrix1, matrix2: matrix): matrix =
    if areAllDimsEqual(matrix1, matrix2):
        result.cols = matrix1.getColumns()
        result.sheragi = matrix1.getRows()
        for i in 0 .. matrix1.getRows() * matrix1.getColumns() - 1:
            result.data.add(matrix1.data[i] - matrix2.data[i])
    else:
        raise newException(IndexDefect, "матрыцы немагчыма адняць(-): не супадаюць размернасці")

#метад множання дадзенай матрыцы на дадзеную лічбу
proc `*`*(matrix: matrix, multiplier: SomeNumber): matrix =
    var mult: float = float(multiplier)
    result.cols = matrix.cols
    result.sheragi = matrix.sheragi
    for i in 0 .. matrix.getRows() * matrix.getColumns() - 1:
        result.data.add(matrix.data[i] * mult)

#дапаможны метад для множання матрыц, праверае ці падыходзят для множання дадзеныя матрыцы праз размернасць (шэрагі) першай матрыцы і размернасць (калонкі) другой матрыцы
proc areEligableForMatrixMult(mat1, mat2: matrix): bool =
    result = false
    if mat1.getRows() == mat2.getColumns():
        result = true

#асноўны метад для множання матрыц, выкарыстоўвае дапаможны метад areEligableForMatrixMult(m1, m2: matrix): bool
proc `*`*(matrix: matrix, multiplier: matrix): matrix =
    if areEligableForMatrixMult(matrix, multiplier):
        result = raspMatrix(matrix.getRows(), multiplier.getColumns())
        for i in 0 .. matrix.getRows() - 1:
            for j in 0 .. multiplier.getColumns() - 1:
                for k in 0 .. matrix.getColumns() - 1:
                    result[i, j] = result[i, j] + matrix[i, k] * multiplier[k, j]
    else: 
        raise newException(IndexDefect, "матрыцы немагчыма перамножыць(*): не супадаюць размернасці")

#метад транспаніравання матрыц, выкарыстоўвае асноўны метад допуска да эляментаў матрыцы па індэксу
proc transpose*(matrix: matrix): matrix =
    result = raspMatrix(matrix.getColumns(), matrix.getRows())
    for i in 0 .. matrix.getRows() - 1:
        for j in 0 .. matrix.getColumns() - 1:
            result[j, i] = matrix[i, j] 

#перарабіць!!!
#дапаможны метад знаходжання адваротнай матрыцы для дадзенай матрыцы маленькіх размернасцяў (шэрагі = 2 і калонкі = 2)
proc smallInv(matrix: matrix): matrix =
    result = raspMatrix(matrix.getRows(), matrix.getColumns())
    var 
        temp1: float
        temp2: float
    for i in 0 .. result.getColumns * result.getRows - 1:
        result.data[i] = matrix.data[i]
    temp1 = result[0, 0]
    temp2 = result[1, 1]
    result[0, 0] = temp2
    result[1, 1] = temp1
    result[0, 1] = result[0,1] * (-1)
    result[1, 0] = result[1, 0] * (-1)

#дапаможны метад знаходжання адваротнай матрыцы для дадзенай матрыцы вялікіх размернасцяў (шэрагі > 2 і калонкі > 2)
proc bigInv(matrix: matrix): matrix = 
    result = raspMatrix(1, 1)

#асноўны метад знаходжання адваротнай матрыцы з выбарам дапаможнага метада па размернасці матрыцы
proc inv*(matrix: matrix): matrix =
    if matrix.getColumns == 2 and matrix.getRows == 2:
        result = smallInv(matrix)
    if matrix.getColumns > 2 and matrix.getRows > 2:
        result = bigInv(matrix)

#метад знаходжання вызначальніка матрыцы
proc det*(matrix: matrix): float =
    result = 0.0

#метад пераводу аб'ектаў класа matrix да класа string, асноўны метад вывядзення дадзеных матрыц
proc matrixToString*(matrix: matrix): string =
    result = "["
    for i in 0 .. matrix.getRows() - 1:
        result.add("[")
        for  j in 0 .. matrix.getColumns() - 1:
            result.add($matrix[i,j])
            if j != matrix.getColumns() - 1:
                result.add(" ")
        result.add("]")
        if i != matrix.getRows() - 1:
            result.add("\n")
    result.add("]")




#фэйлы і тэсты 


# var md = @[0.0, 1.0, 2.0, 3.0]
# var mdd = @[2.0, 3.0, 4.0, 5.0]
# var mat: matrix = raspMatrix(2, 2, md)
# var mat2: matrix = raspMatrix(2, 2, mdd)
# echo (mat * mat2).matrixToString()
# echo mat

#першая версія асноўнага метаду вывядзення дадзенай матрыцы
# proc failMatrixToString(matrix: matrix): string =
#     result = "["
#     var i: int = 0
#     for j in 0 .. matrix.getRows() - 1:
#         result.add("[")
#         i = 0
#         while i < matrix.getColumns() - 1:
#             echo matrix[i, j]
#             result.add($matrix[i, j])
#             i = i + 1
#         result.add("]\n")

# #тэст асноўнага метаду вывядзення дадзенай матрыцы
# proc testMatrixToString(matrix: matrix): void =
#     for i in 0 .. matrix.getRows() - 1:
#         for j in 0 .. matrix.getColumns() - 1:
#             echo "i ", i, " ", matrix.getRows
#             echo "j ", j, " ", matrix.getColumns
#             echo matrix[i, j]

# #другая версія асноўнага метаду вывядзення дадзенай матрыцы
# proc InWorkMatrixToString*(matrix: matrix): string =
#     result = "["
#     for i in 0 .. matrix.getRows() - 1:
#         result.add("[")
#         for  j in 0 .. matrix.getColumns() - 1:
#             result.add($matrix[i,j])
#             result.add(" ")
#         result.add("]\n")
#         # if i != matrix.getRows() - 1:
#         #     result.add("\n")
#     result.add("]")