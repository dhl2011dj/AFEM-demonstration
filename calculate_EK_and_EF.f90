!****************************************************************************************************
!这是原子有限元的子程序，作用在于计算单元刚度阵和力列阵的子程序：
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
!subroutine calculate_EK_and_EF(coordinate_node_of_element, amount_node_of_element, K_element, element_force)
subroutine calculate_EK_and_EF(coordinate_node_of_element, K_element, element_force)

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

!本程序中：
!不需要将EX变成X，因此：
!coordinate_node_of_element =>  EX  =>  X
!不需要将NELE变成NSE，因此：
!node_number_of_each_element=>  NELE=>  NSE


!使用afem_parameter模块（其中定义了参数）
use afem_parameter


!默认情况下需要声明变量
implicit none

    !定义变量
    
    !node_number_of_each_element(i)存储第i个单元中的amount_node_of_element个节点号
    integer(kind=4)node_number_of_each_element(amount_node_of_element)
    
    !coordinate_node_of_element(i, j)存储每个单元中i号节点的坐标值j = 1, 3分别代表将i号节点坐标的X、Y、Z值
    !amount_node_of_element = 2
    real(kind=8) coordinate_node_of_element(amount_node_of_element,3)
    !单元的力列阵，K_element_row = 6
    real(kind=8) element_force(K_element_row)
    !单元刚度矩阵，K_element_column = 6
    !研究对势单元刚度阵暂时不需要这么大，K_element_row = 6,K_element_column = 6
    !real(kind=8) K_element(30,30)
    real(kind=8) K_element(K_element_row,K_element_column)
    
    !定义循环变量
    integer(kind=4) i, j, m
    
    K_element = 0.0
    element_force = 0.0
    
    
    !更新单元力列阵：F + ?u/?x
    do m =1, 3
        element_force(m) = element_force(m) + stiffness * (dabs(coordinate_node_of_element(1, m) - coordinate_node_of_element(2, m)))
    enddo
    
    !形成单元刚度阵
    !m为与(3*(i - 1) + j)相辅助的变量，用于给单元刚度阵的对称位置赋值
    !m的最大值应该是3倍的amount_node_of_element = 2
    do m = 1, 3 * amount_node_of_element
        !按单元中的节点号循环
        do i = 1, amount_node_of_element
            !对势中没什么用
            !if((i.EQ.1).OR.(node_number_of_each_element(i).NE.node_number_of_each_element(1))) then
                !按X、Y、Z方向坐标循环
                do j = 1, 3
                    !先生成左下三角中的元素，利用对称形成右上三角的元素
                    if(m.GE.(3*(i - 1) + j)) then
                        !如果形成的是单元中1号节点（单元内序号），则节点刚度加入势能u对位置x的二阶偏导数：?2u/?x1?x1
                        if (m.EQ.(3*(i - 1) + j)) then
                            K_element(m,(3*(i - 1) + j)) = K_element(m,(3*(i - 1) + j)) + stiffness
                        !如果形成的是单元中2号节点（单元内序号）（对势），则节点刚度加入势能u对位置x的二阶偏导数：?2u/?x?x
                        else if((m-3).EQ.(3*(i - 1) + j)) then
                            K_element(m,(3*(i - 1) + j)) = K_element(m,(3*(i - 1) + j)) - stiffness
                        endif
                        !单元刚度阵为对称阵，给对称位置元素赋值
                        K_element((3*(i - 1) + j),m) = K_element(m,(3*(i - 1) + j))
                    else
                        !跳过循环
                        cycle
                    endif
                enddo
                !按X、Y、Z方向坐标循环结束
            !endif
        enddo
        !按单元中的节点号循环结束
    enddo
    !形成单元刚度阵结束
    !write(*,"('单元刚度矩阵 K_element = ')")
    do j = 1, K_element_column
        !write(*,"(1X, 6F9.3)") (K_element(i,j),i = 1, K_element_row)
    enddo

end