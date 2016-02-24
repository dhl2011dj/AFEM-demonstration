!****************************************************************************************************
!这是原子有限元的子程序，作用在于调用集中、分布载荷子程序进行边界条件数据的输入和存储，输入格式和数据如下：
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
subroutine loads_input()
!使用afem_parameter模块（其中定义了参数）
use afem_parameter

!默认情况下需要声明变量
implicit none

    !定义变量
    
    !定义系数矩阵的行数
    integer(kind=4) number_row_K_global
    
    !该变量用于存储载荷边界条件的类型以及判断边界条件是否输入完毕
    character(len=4) judge_end_boundary_condition
    
    !作用于节点上的集中载荷（坐标方向）的个数
    integer(kind=4) number_force
    
    !作用于节点上的分布载荷（坐标方向）的个数
    integer(kind=4) number_pressure
    
    !为集成的全局节点力向量分配内存
    allocate(force_of_node(DOF + 3*amount_node_clear))
    !赋初始值
    force_of_node = 0.0
    
    !系数矩阵的行数即全局自由度数
    number_row_K_global = DOF
    
    !write(*,"(/,'2.**************************************************************',/,1X,'开始输入边界条件：',/)")
    !
    write(*,"(1X,'请输入边界条件的类型：',/,1X,'[a]：集中载荷边界条件，[b]：分布载荷边界条件')")
    write(*,"(1X,'如果需要边界条件输入完毕请输入：exit',/)")
    !边界条件的输入，只有当输入字符“exit”的时候循环退出，否则一直要求输入载荷边界条件
    do while(.true.)
        
        read(*,*) judge_end_boundary_condition
        if(judge_end_boundary_condition.NE.'exit') then
            !如果judge_end_boundary_condition = 1说明要输入集中载荷
            if(judge_end_boundary_condition.EQ.'a') then
                write(*,"(/,1X,'请输入力的个数：number_force')")
                read(*,*) number_force
                
                !调用子程序进行集中载荷数据的输入和文件保存
                call concentrated_loads_input(number_force)
                
            !如果judge_end_boundary_condition = 2说明要输入均布载荷
            elseif(judge_end_boundary_condition.EQ.'b') then
                write(*,"(1X,'暂不支持均布载荷的输入',/)")
                !调用子程序进行均布载荷数据的输入和文件保存
                !call distributed_loads_input(number_pressure)
                !调用子程序求解均布载荷数据的等效节点力并将等效节点力存入外力矩阵
                !call distributed_loads_assembling(number_pressure, number_row_K_global)
            else
                write(*,"(1X,'您的输入有误，请重新输入',/)")
            endif
            
        elseif(judge_end_boundary_condition.EQ.'exit') then
            exit
        endif
    enddo
    !
    !call 
    
    !write(*,"(/,1X,'边界条件输入结束。',/,'2.**************************************************************',/)")
    
    
end