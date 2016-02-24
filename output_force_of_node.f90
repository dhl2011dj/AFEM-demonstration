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
subroutine output_force_of_node()

!使用afem_parameter模块（其中定义了参数）
use afem_parameter

!默认情况下需要声明变量
implicit none

    !定义变量
    
    !定义循环变量
    integer(kind=4) i, j, k
    
    !定义逻辑变量
    logical judge_node_clear
    !赋初始值
    judge_node_clear = .true.
    !k是将force_of_node中的有效节点力放入orce_of_node_cleared时的循环变量
    k = 0
    
    !为force_of_node_cleared分配内存空间
    allocate(force_of_node_cleared(DOF))
    
    open(1,file = 'force_of_node_cleared.txt')
    
    write(1,"(1X, '全局力列阵 force_of_node_cleared = ')")
    do i = 1, DOF + 3*amount_node_clear
        
        !重置变量
        judge_node_clear = .true.
        
        !纯输出force_of_node格式
        !write(1,"(1X, F9.3)") force_of_node(i)
        
        !判断原子是否为离散原子
        do j = 1, amount_node_clear
            if((int((i-1)/3)+1) .EQ. number_node_clear(j)) then
                judge_node_clear = .false.
            endif
        enddo
        
        !根据上个循环产生的judge_node_clear来判断是否输出
        if(judge_node_clear) then
            
            k = k + 1
            force_of_node_cleared(k) = force_of_node(i)
            !带原子序号和方向序号的输出格式
            write(*,"(1X, I5, '原子', I2, '方向力：', F9.3)") (int((i-1)/3)+1), (i-3*int((i-1)/3)), force_of_node(i)
            write(1,"(1X, I5, '原子', I2, '方向力：', F9.3)") (int((i-1)/3)+1), (i-3*int((i-1)/3)), force_of_node_cleared(k)
            
        endif
    enddo
    
    close(1)
    
end