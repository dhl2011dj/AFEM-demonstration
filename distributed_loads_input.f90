!****************************************************************************************************
!这是原子有限元的子程序，作用在于输入分布载荷数据并保存至文件，输入格式和数据如下：
!
!读入变量（input）：
!
!输出变量（output）：
!
!新建变量（new）：
!
!调用子程序（函数）：
!
!被调用子程序（函数）：
!
!读写文件名：
!
!****************************************************************************************************
subroutine distributed_loads_input(number_pressure)

!本程序中变量对应刘老师代码中给出的变量
!number_pressure       =>  NP
!information_pressure  =>  MP
!value_pressure        =>  ZP

!默认情况下需要声明变量
implicit none

    !定义变量
    
    !作用于节点上的分布载荷（坐标方向）的个数
    integer(kind=4) number_pressure
    
    !作用于节点上的分布载荷的信息数组
    !information_pressure(1-2,number_pressure)：按逆时针顺序存储均布载荷施加的节点编号
    !information_pressure(3,i) = 0：作用X方向的均布力
    !information_pressure(3,i) = 1：作用Y方向的均布力
    !information_pressure(3,i) = 2：？？？？？？？？？
    !information_pressure(4,i)：第i个载荷组
    integer(kind=4) information_pressure(4, number_pressure)
    
    !定义循环变量
    integer(kind=4) i, j
    
    !作用分布力的值（以坐标方向为正）
    real(kind=8) value_pressure(number_pressure)
    
    !读取存有分布载荷个数、信息和值的文件
    do i = 1, number_pressure
        !read(unit=5,fmt=*)代表从键盘输入
        read(5,*)(information_pressure(j,i), j = 1,2), value_pressure(i)
    enddo
    
    !输出字符串'34HCONCENTRATED LOADS information_pressure(3,NF) value_pressure(NF)'
    !？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？
    !应该打开定义的设备号为6的文件：open(unit=6, file='results.txt')
    write(6,"(/,10X,'29HUNIFORM LOADS MP(4,NP) ZP(NP)')")
    !？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？
    
    !输出所有分布力对应的：
    !information_pressure(1-2,number_pressure)：按逆时针顺序存储均布载荷施加的节点编号
    !information_pressure(3,i) = 0：作用X方向的均布力
    !information_pressure(3,i) = 1：作用Y方向的均布力
    !information_pressure(3,i) = 2：？？？？？？？？？
    !information_pressure(4,i)：第i个载荷组
    do i = 1, number_pressure
        !？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？
        !应该打开定义的设备号为6的文件：open(unit=6, file='results.txt')
        write(6,"(/,2I10,F20.6)") (information_pressure(j,i),j=1,2), value_pressure(i)
        !？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？
    enddo
    
end