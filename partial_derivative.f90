!****************************************************************************************************
!这是原子有限元的子程序，作用在于计算势能关于位置的偏导数：
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
subroutine partial_derivative(coordinate_node_of_element, node_number_of_each_element, p_u_p_x, pp_u_pp_x)

!使用afem_parameter模块（其中定义了参数）
!单元刚度阵的行数K_element_row = 6，单元刚度阵的列数K_element_column = 6，amount_node_of_element = 2
use afem_parameter

!默认情况下需要声明变量
implicit none

    !定义变量
    
    !node_number_of_each_element(i)存储第i个单元中的amount_node_of_element个节点号
    integer(kind=4)node_number_of_each_element(amount_node_of_element)
    
    !coordinate_node_of_element(i, j)存储每个单元中i号节点的坐标值j = 1, 3分别代表将i号节点坐标的X、Y、Z值
    !amount_node_of_element = 2
    real(kind=8) coordinate_node_of_element(amount_node_of_element,3)
    
    !表示势能u对位置x的一阶偏导数：∂u/∂x
    real(kind=8) p_u_p_x(1,3)
    !表示势能u对位置x的二阶偏导数：∂2u/∂x∂x
    real(kind=8) pp_u_pp_x(1,3,6,3)
    
    write(*,"('由于使用了对势，线性弹簧，刚度矩阵中的元素都为常数，因此暂时不需要求偏导数')")
    
end