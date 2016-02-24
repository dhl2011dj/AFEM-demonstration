!****************************************************************************************************
!这是原子有限元的子程序，作用在于组装全局刚度阵：
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
subroutine K_global_assembling()

!本程序中变量对应刘老师代码中给出的变量
!number_element             =>  IO
!amount_all_elements        =>  NE      
!amount_all_nodes           =>  NG
!node_of_element            =>  IJM
!                           =>  TE
!coordinate_nodes_initial   =>  XY
!coordinate_nodes_deformed  =>  CXY
!element_force              =>  EF
!current_element_force      =>  CFE
!coordinate_node_of_element =>  EX
!node_number_of_each_element=>  NELE
!K_element                  =>  EK


!使用afem_parameter模块（其中定义了参数）
!单元刚度阵的行数K_element_row = 6，单元刚度阵的列数K_element_column = 6
use afem_parameter

!默认情况下需要声明变量
implicit none

    !定义变量
    
    !单元序号
    integer(kind=4) number_element
    
    !TE
    !integer(kind=4) TE(number_element)
    !node_number_of_each_element(i)存储第i个单元中的amount_node_of_element个节点号
    integer(kind=4) node_number_of_each_element(amount_node_of_element)
    
    !真实的全局自由度
    integer(kind=4) DOF_global
    
    !定义程序中的循环变量
    integer(kind=4) i, j, k, m, n, a, b, c, d
    
    !节点坐标数组
    !coordinate_nodes_initial(1,I)代表初始时第I个节点的X坐标
    !coordinate_nodes_initial(2,I)代表初始时第I个节点的Y坐标
    !coordinate_nodes_initial(3,I)代表初始时第I个节点的Z坐标
    real(kind=8) coordinate_nodes_initial(3, DOF/3)
    !变形后节点坐标数组
    !coordinate_nodes_deformed(1,I)代表变形后第I个节点的X坐标
    !coordinate_nodes_deformed(2,I)代表变形后第I个节点的Y坐标
    !coordinate_nodes_deformed(3,I)代表变形后第I个节点的Z坐标
    real(kind=8) coordinate_nodes_deformed(3, DOF/3)
    !现在的单元力列阵
    real(kind=8) current_element_force(K_element_row)
    !单元刚度矩阵
    !K_element_row = 6,K_element_column = 6
    real(kind=8) K_element(K_element_row,K_element_column)
    
    !coordinate_node_of_element(i, j)存储每个单元中i号节点的坐标值j = 1, 3分别代表将i号节点坐标的X、Y、Z值
    !amount_node_of_element = 2
    real(kind=8) coordinate_node_of_element(amount_node_of_element,3)
    !单元的力列阵
    real(kind=8) element_force(K_element_row)
    
    !为全局刚度阵分配内存空间
    !DOF = DOF - 3*amount_node_clear; allocate(K_global(DOF, DOF))
    !DOF_global = DOF - 3*amount_node_clear; allocate(K_global(DOF_global, DOF_global))
    
    !##############################################################程序并行开始块
    !定义并行计算变量
    integer(kind=4) nthreads, tid, omp_get_num_threads, omp_get_thread_num, chunksize, chunk
    parameter (chunksize = 10)
    !定义CPU占用计时变量
    real(kind=8) time_begin, time_end
    
    open(1, file = 'time_K_global_assembling.txt')
    
    !CPU计时程序（计时开始）
    call CPU_TIME(time_begin)
    
    chunk = chunksize
    
    !openMP开始
    !$omp parallel shared(nthreads) private(i,j,number_element)
    !##############################################################程序并行开始块
    
    !赋初始值
    K_global = 0.0
    
    tid = omp_get_thread_num()
    
    if (tid .eq. 0) then
    nthreads = omp_get_num_threads()
    print *, 'number of threads =', nthreads
    end if
    print *, 'thread',tid,' starting...'
    
    !$omp do schedule(dynamic,chunk)
    
    do number_element = 1, amount_all_elements
        write(*,"(1X, 'amount_all_elements = ', I5)") amount_all_elements
        !调用子程序生成单元刚度阵
        !call K_element_assembling(number_element, amount_all_elements, DOF/3, node_of_element, TE, coordinate_nodes_initial, coordinate_nodes_deformed, current_element_force, K_element)
        call K_element_assembling(number_element, K_element)
        !将单元刚度阵组合成为全局刚度阵
        !将第number_element个单元中节点的节点号赋值给node_number_of_each_element数组
        node_number_of_each_element(:) = main_node_of_element(:, number_element)
        
        !将单元中的两个节点号分别赋给i, j
        i = node_number_of_each_element(1)
        j = node_number_of_each_element(2)
        
        !write(*,"(1X, 'i = ', I5)") i
        !write(*,"(1X, 'j = ', I5)") j
        
        !每次进行新单元，重新给c、d赋值
        c = 0
        d = 0
        
        !如果节点编号超过要清除的节点号就减去
        do a = 1, amount_node_clear
            if(i .GT. number_node_clear(a)) then
                c = c + 1
            !elseif(i .EQ. number_node_clear(a)) then
            !    i = 1
            endif
        enddo
        i = i - c
        
        !如果节点编号超过要清除的节点号就减去
        do b = 1, amount_node_clear
            if(j .GT. number_node_clear(b)) then
                d = d + 1
            !elseif(j .EQ. number_node_clear(b)) then
            !    j = 1
            endif
        enddo
        j = j - d
        
        !write(*,"(1X, 'i* = ', I5)") i
        !write(*,"(1X, 'j* = ', I5)") j
        
        !m, n分别是i节点和j号节点的X/Y/Z方向的循环变量
        do m = 1, 3
            do n = 1, 3
                K_global(3*(i-1)+m, 3*(i-1)+n) = K_global(3*(i-1)+m, 3*(i-1)+n) + K_element(m, n)
            enddo
        enddo
        
        !m, n分别是i节点和j号节点的X/Y/Z方向的循环变量
        do m = 1, 3
            do n = 1, 3
                K_global(3*(i-1)+m, 3*(j-1)+n) = K_global(3*(i-1)+m, 3*(j-1)+n) + K_element(m, 3+n)
            enddo
        enddo
        
        !m, n分别是i节点和j号节点的X/Y/Z方向的循环变量
        do m = 1, 3
            do n = 1, 3
                K_global(3*(j-1)+m, 3*(i-1)+n) = K_global(3*(j-1)+m, 3*(i-1)+n) + K_element(3+m, n)
            enddo
        enddo
        
        !m, n分别是i节点和j号节点的X/Y/Z方向的循环变量
        do m = 1, 3
            do n = 1, 3
                K_global(3*(j-1)+m, 3*(j-1)+n) = K_global(3*(j-1)+m, 3*(j-1)+n) + K_element(3+m, 3+n)
            enddo
        enddo
    enddo
    
    !$omp enddo nowait
    
    !输出全局刚度阵的维度
    write(*,"('全局刚度矩阵的维度为：', I10, '          *', I10)") DOF, DOF
    
    print *, 'thread',tid,' done.'
    
    !##############################################################程序并行结束块
    !openMP结束
    !$omp end parallel
    
    !CPU计时程序（计时结束）
    call CPU_TIME(time_end)
    
    !输出CPU占用时间
    write(1,"(1X, 'K_global_assembling子程序总共占用CPU时间为：', F, 's')") time_end - time_begin
    
    close(1)
    !##############################################################程序并行结束块
end