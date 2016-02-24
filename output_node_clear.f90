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
subroutine output_node_clear()

!使用afem_parameter模块（其中定义了参数）
use afem_parameter

!默认情况下需要声明变量
implicit none

    !定义变量
    
    !定义循环变量
    integer(kind=4) i
    
    open(1,file = 'node_clear.txt')
    
    !输出共需要清除多少个节点
    write(1,"(1X, '共需要清除', I3, '个节点：')") amount_node_clear
    
    !输出需要清除的原子
    do i = 1, amount_node_clear
        write(1,"(1X, I10)") number_node_clear(i)
    enddo
    
    close(1)
    
end