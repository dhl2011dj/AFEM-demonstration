!****************************************************************************************************
!这是原子有限元的子程序，作用在于形成单元刚度阵：
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
subroutine K_element_assembling(number_element, K_element)
!对应于刘老师代码中的子程序KE

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
    
    !单元序号：按单元循环的单元号，来自子程序K_global_assembling
    integer(kind=4) number_element
    
    !TE
    !暂时不知道这玩意什么用？先从形参中拿掉
    !integer(kind=4) TE(number_element)
    !node_number_of_each_element(i)存储第i个单元中的amount_node_of_element个节点号
    integer(kind=4) node_number_of_each_element(amount_node_of_element)
    
    !定义程序中的循环变量
    integer(kind=4) i, j
    
    !节点坐标数组
    !coordinate_nodes_initial(1,I)代表初始时第I个节点的X坐标
    !coordinate_nodes_initial(2,I)代表初始时第I个节点的Y坐标
    !coordinate_nodes_initial(3,I)代表初始时第I个节点的Z坐标
    real(kind=8) coordinate_nodes_initial(3, DOF/3 + amount_node_clear)
    !变形后节点坐标数组
    !coordinate_nodes_deformed(1,I)代表变形后第I个节点的X坐标
    !coordinate_nodes_deformed(2,I)代表变形后第I个节点的Y坐标
    !coordinate_nodes_deformed(3,I)代表变形后第I个节点的Z坐标
    real(kind=8) coordinate_nodes_deformed(3, DOF/3 + amount_node_clear)
    !现在的单元力列阵
    !暂时从形参中拿掉
    real(kind=8) current_element_force(K_element_row)
    !单元刚度矩阵，K_element_column = 6
    !研究对势单元刚度阵暂时不需要这么大，K_element_row = 6,K_element_column = 6
    !real(kind=8) K_element(30,30)
    real(kind=8) K_element(K_element_row,K_element_column)
    
    !coordinate_node_of_element(i, j)存储每个单元中i号节点的坐标值j = 1, 3分别代表将i号节点坐标的X、Y、Z值
    !amount_node_of_element = 2
    real(kind=8) coordinate_node_of_element(amount_node_of_element,3)
    !单元的力列阵
    real(kind=8) element_force(K_element_row)
    
    !赋初始值
    coordinate_nodes_initial = node_position
    coordinate_nodes_deformed = node_position
    
    write(*,"(/, '*** subroutine - K_element_assembling - begin ***', /)")
    
    !显示单元编号
    write(*,"('*********** ', I10, '号单元', '      ***********')") number_element
    
    !将每个单元中i号节点的坐标值（j = 1, 3分别代表将i号节点坐标的X、Y、Z值）存入数组coordinate_node_of_element(i, j)
    !amount_node_of_element = 2
    do i = 1, amount_node_of_element
        !j = 1, 3代表将节点坐标的X、Y、Z值放入coordinate_node_of_element(i, j)中
        do j = 1, 3
            coordinate_node_of_element(i, j) = coordinate_nodes_deformed(j, main_node_of_element(i, number_element))
            !输出单元中节点的X/Y/Z方向坐标
            write(*,"(1X, 'coordinate_node_of_element(', I3, ',', I3, ')', ' = ', F10.7)") i, j, coordinate_node_of_element(i, j)
        enddo
    enddo
    
    !将第number_element个单元中节点的节点号赋值给node_number_of_each_element数组
    node_number_of_each_element(:) = main_node_of_element(:, number_element)
    
    !调用计算单元刚度阵和力列阵的子程序
    call calculate_EK_and_EF(coordinate_node_of_element, K_element, element_force)
    
    current_element_force = element_force
    
    write(*,"(/, '**** subroutine - K_element_assembling - end ****', /)")
    
end