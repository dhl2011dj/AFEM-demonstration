subroutine output_solution()

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
    
    
    open(1,file = 'X.txt')
    
    write(1,"(1X, '原子位移解向量 X = ')")
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
            !带原子序号和方向序号的输出格式
            write(*,"(1X, I5, '原子', I2, '方向位移：', F9.3)") (int((i-1)/3)+1), (i-3*int((i-1)/3)), X(k)
            write(1,"(1X, I5, '原子', I2, '方向位移：', F9.3)") (int((i-1)/3)+1), (i-3*int((i-1)/3)), X(k)
            
        endif
    enddo
    
    close(1)
    
end