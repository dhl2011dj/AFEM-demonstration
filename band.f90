!****************************************************************************************************
!这是原子有限元的子程序，作用在于利用列选主元Gauss消去法(列主元)求解线性方程组：
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
subroutine solution_band(number_row_K_global, number_column_K_global, K_global, force_of_node, )

!本程序中变量对应刘老师代码中给出的变量
!number_row_K_global            =>  NT
!number_column_K_global         =>  ND
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

!默认情况下需要声明变量
implicit none

    !定义变量
    
    !定义系数矩阵的行数
    integer(kind=4) number_row_K_global
    !定义系数矩阵的列数
    integer(kind=4) number_column_K_global
    
    !全局刚度阵
    real(kind=8) K_global(number_row_K_global, number_column_K_global)
    !全局力列阵
    real(kind=8) force_of_node(number_row_K_global)
    
    !定义循环变量
    integer(kind=4) i, j, m, n
        
    

end