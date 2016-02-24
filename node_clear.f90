!****************************************************************************************************
!这是原子有限元的子程序，作用在于清除没有形成单元的节点：
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
subroutine node_clear()

!使用afem_parameter模块（其中定义了参数）
use afem_parameter

!默认情况下需要声明变量
implicit none

    !定义变量
    
    !判断是否要清除无用节点
    integer(kind=4) judge_node_clear
    
    !定义循环变量
    integer(kind=4) i, j, k
    
    !为清除节点的数组分配内存空间
    allocate(number_node_clear(int(DOF/3)))
    
    !赋初始值
    judge_node_clear = 0
    number_node_clear = 0
    !清除节点
    !循环所有原子序号
    amount_node_clear = 0
    do k = 1, DOF/3
        
        !置零
        judge_node_clear = 0
        
        !单元中的第j个原子
        do j = 1, amount_node_of_element
            !单元中节点号的循环，第i个单元
            do i = 1, amount_all_elements
                
                !如果原子序号存在于单元中则不删除，否则删除
                if(k .EQ. main_node_of_element(j,i)) then
                    !需要清除节点的下标增加
                    judge_node_clear = judge_node_clear + 1
                    write(1,"(1X, 'judge_node_clear = ', I3)") judge_node_clear
                    exit
                endif
                
            enddo
            
            !如果judge_node_clear不为0则跳过循环
            if(judge_node_clear .NE. 0) then
                !write(*,"(1X, 'k = ', I3)") k
                exit
            endif
            
        enddo
        
        if(judge_node_clear .EQ. 0) then
            amount_node_clear = amount_node_clear + 1
            !将k号原子放入要清除原子的数组中
            number_node_clear(amount_node_clear) = k
        endif
    enddo
    !输出共需要清除多少个节点
    write(*,"(/, 1X, '共需要清除', I3, '个节点')") amount_node_clear
    !输出需要清除的原子
    write(*,"(1X, '需要清除掉的孤立原子：')")
    write(*,*)(number_node_clear(i),i = 1,amount_node_clear)
    
    !输出
    call output_node_clear()
    
end