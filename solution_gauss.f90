!****************************************************************************************************
!这是原子有限元的子程序，作用在于利用主选元Gauss消去法(列主元)求解线性方程组：
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
subroutine solution_gauss()

!本程序中变量对应刘老师代码中给出的变量
!DOF            =>  NT
!DOF         =>  ND
!number_given_dislpacement      =>  NB
!information_given_dislpacement =>  MB
!value_given_dislpacement       =>  ZB
!K_global_bandwidth             =>  SK
!force_of_node                  =>  F
!coordinate_nodes_initial       =>  XY
!coordinate_nodes_deformed      =>  CXY
!                               =>  FORCE
!                               =>  FACTOR
!                               =>  FF
!                               =>  ERRF

!m                              =>  M
!n                              =>  N
!mn                             =>  NN

!使用afem_parameter模块（其中定义了参数）
use afem_parameter
!使用inverse_matrix模块，其中包含了高斯消去法求系数矩阵逆矩阵及求解有限元方程的子程序
use inverse_matrix

!默认情况下需要声明变量
implicit none

    !定义变量
    
    !全局刚度阵
    !real(kind=8) K_global(DOF, DOF)
    
    !定义节点位移向量
    real(kind=8) displacement_of_node(DOF)
    
    !全局力列阵
    real(kind=8) force_of_node_2(DOF)
    
    !定义循环变量
    integer(kind=4) i, j, m, n
    
    !定义CPU占用计时变量
    real(kind=8) time_begin, time_end
    
    open(1, file = 'time_solution_gauss.txt')
    
    !CPU计时程序（计时开始）
    call CPU_TIME(time_begin)
    
    !为force_of_node_cleared分配内存空间
    allocate(X(DOF))
    
    !采用高斯消去法求解有限元方程matrix_gauss(A,B,X,n,m)
    !子程序matrix_gauss(A,B,X,n,m)在
    force_of_node_2 = force_of_node
    
    call matrix_gauss(K_global, force_of_node_cleared, displacement_of_node, DOF, 1)
    
    !赋值
    X = displacement_of_node
    
    !CPU计时程序（计时结束）
    call CPU_TIME(time_end)
    
    !输出CPU占用时间
    write(1,"(1X, 'solution_gauss子程序总共占用CPU时间为：', F, 's')") time_end - time_begin
    
    close(1)
    
end