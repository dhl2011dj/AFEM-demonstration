!****************************************************************************************************
!这是原子有限元的子程序，作用在于组装集中载荷数据并存入外力矩阵，输入格式和数据如下：
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
subroutine concentrated_loads_assembling(number_force, information_force, value_force)

!使用afem_parameter模块（其中定义了参数）
use afem_parameter

!本程序中变量对应刘老师代码中给出的变量
!number_force       =>  NF
!information_force  =>  MF
!value_force        =>  ZF
!number_row_K_global=>  NT
!force_of_node      =>  F

!默认情况下需要声明变量
implicit none

    !定义变量
    
    !作用于节点上的集中载荷（坐标方向）的个数
    integer(kind=4) number_force
    
    !作用于节点上的集中载荷的信息数组
    !information_force(1,i)：第i个集中载荷作用的节点号
    !information_force(2,i) = 1：作用X方向的集中力
    !information_force(2,i) = 2：作用Y方向的集中力
    !information_force(2,i) = 3：作用Y方向的集中力
    integer(kind=4) information_force(2, number_force)
    
    !定义循环变量
    integer(kind=4) i, j
    
    !作用集中力的值（以坐标方向为正）
    real(kind=8) value_force(number_force)
    
    write(*,"(/, 1X, '********** 集中载荷开始放入全局力列阵 **********')")
    
    do i = 1, number_force
        !j代表第i个集中载荷作用的节点号
        j = information_force(1,i)
        !force_of_node是行数为number_row_K_global的力列阵
        !每个节点存在3个方向的力和位移因此
        !如果information_force(2,i) = 
        !？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？
        force_of_node(3*(j - 1) + information_force(2,i)) = force_of_node(3*(j - 1) + information_force(2,i)) + value_force(i)
        !书上规定information_force(2,i) = 1代表X方向集中力 = 0代表Y方向集中力，感觉有问题，或者至少与本程序不符合
        !可能为如下表述：
        !1.force_of_node(3*(j - 1) + information_force(2,i)) = force_of_node(3*(j - 1) + information_force(2,i)) + value_force(i)
        !很奇怪，为什么没有Z方向的集中作用力？
        !并且上式中information_force(2,i) = 1代表X方向集中力 = 2代表Y方向集中力 = 3代表Z方向集中力
        
        !或者
        !2.force_of_node(3*j - information_force(2,i)) = force_of_node(3*j - information_force(2,i)) + value_force(i)
        !很奇怪，为什么没有Z方向的集中作用力？
        !并且上式中information_force(2,i) = 2代表X方向集中力 = 1代表Y方向集中力 = 0代表Z方向集中力
        !方式2与书上有些类似
        !？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？？
    enddo
    
    write(*,"(/, 1X, '- \ | / - \ | / - \ | / - \ | / - \ | /')")
    write(*,"(/, 1X, '********** 集中载荷放入全局力列阵结束 **********', ///)")
    
    write(*,"(/, 1X, '现在您可以选择输入‘exit’来结束集中载荷的输入')")
    write(*,"(1X, '或者继续输入边界条件的类型：[a]：集中载荷边界条件，[b]：分布载荷边界条件', /)")
    
end