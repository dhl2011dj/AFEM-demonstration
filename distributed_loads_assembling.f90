!****************************************************************************************************
!这是原子有限元的子程序，作用在于求解均布载荷数据的等效节点力并将等效节点力存入外力矩阵，输入格式和数据如下：
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
subroutine distributed_loads_assembling(number_pressure, number_row_K_global)

!本程序中变量对应刘老师代码中给出的变量
!number_pressure       =>  NP
!information_pressure  =>  MP
!value_pressure        =>  ZP

!默认情况下需要声明变量
implicit none

    !定义变量
    
    !系数矩阵的行数
    integer(kind=4) number_row_K_global
    
    !作用于节点上的分布载荷（坐标方向）的个数
    integer(kind=4) number_pressure
    
    !作用于节点上的分布载荷的信息数组
    !information_pressure(1,i)：第i个分布载荷作用的节点号
    !information_pressure(2,i) = 1：作用X方向的分布力
    !information_pressure(2,i) = 0：作用Y方向的分布力
    integer(kind=4) information_pressure(2, number_pressure)
    
    !定义循环变量
    integer(kind=4) i, j
    
    !作用分布力的值（以坐标方向为正）
    real(kind=8) value_pressure(number_pressure)
    
    write(*,"(/,1X,'“distributed_load_input”：本子程序暂时不支持调用。')")
        
end