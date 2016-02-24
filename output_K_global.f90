!****************************************************************************************************
!这是原子有限元的子程序，作用在于保存全局刚度阵至文件，输入格式和数据如下：
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
subroutine output_K_global()

!使用afem_parameter模块（其中定义了参数）
use afem_parameter

!默认情况下需要声明变量
implicit none

    !定义变量
    !真实的全局自由度
    integer(kind=4) DOF_global
    
    !定义循环变量
    integer(kind=4) i, j, k
    
    !DOF = DOF - 3*amount_node_clear
    DOF_global = DOF
    
    open(1,file = 'K_global.txt')
    
    write(1,"(1X, '全局刚度矩阵 K_global = ')")
    do j = 1, DOF_global
        write(1,"(1X, 42F9.3)") (K_global(i,j),i = 1, DOF_global)
    enddo
    
    close(1)
    
end