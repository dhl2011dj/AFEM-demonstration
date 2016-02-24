!****************************************************************************************************
!这是原子有限元的子程序，作用在于利用边界条件（零位移值）消除总刚奇异性，输入格式和数据如下：
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
subroutine boundary_conditions_introduced_1(number_given_displacement)

!本程序中变量对应刘老师代码中给出的变量
!DOF                            =>  NT
!DOF                            =>  ND
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
    
    !給定位移条件的个数
    integer(kind=4) number_given_displacement
    !给定位移边界条件的信息
    !information_given_displacement(1,i)：第i个固定位移的节点号
    !information_given_displacement(2,i) = 1：X方向给定位移
    !information_given_displacement(2,i) = 2：Y方向给定位移
    !information_given_displacement(2,i) = 3：Z方向给定位移
    integer(kind=4) information_given_displacement(2, number_given_displacement)
    
    !
    integer(kind=4) S
    !定义循环变量
    integer(kind=4) i, j
    !变量m存储第i个载荷作用的方向，变量n存储第i个载荷作用的节点号
    integer(kind=4) m, n, a, b, c, d
    !变量mn存储载荷对应节点、对应方向的坐标
    !变量mn对应coordinate_nodes_deformed、coordinate_nodes_initial数组中n号节点、m方向的坐标
    integer(kind=4) mn

    
    !给定位移边界条件的值
    !本子程序boundary_conditions_introduced_1中都是零位移利用对角元素改一法消除刚度阵奇异性
    real(kind=8) value_given_displacement(number_given_displacement)

    
    !赋初始值
    information_given_displacement = 0
    !固定位移：1原子X方向
    information_given_displacement(1,1) = 1
    information_given_displacement(2,1) = 1
    !固定位移：9原子Y方向
    !information_given_displacement(1,2) = 9
    !information_given_displacement(2,2) = 2
    !固定位移：17原子Z方向
    information_given_displacement(1,3) = 12
    information_given_displacement(2,3) = 3
    !固定位移：1原子Y方向
    information_given_displacement(1,2) = 15
    information_given_displacement(2,2) = 2
    !固定位移：1原子Y方向
    !information_given_displacement(1,5) = 1
    !information_given_displacement(2,5) = 3
    !固定位移：1原子Y方向
    !information_given_displacement(1,2) = 9
    !information_given_displacement(2,2) = 3
    
    !本子程序boundary_conditions_introduced_1中都是零位移利用对角元素改一法消除刚度阵奇异性
    value_given_displacement(number_given_displacement) = 0.0
    
    !第一层循环：
    !1==============================================================================
    do i = 1, number_given_displacement
        !将第i个载荷作用的节点号赋值给变量n
        n = information_given_displacement(1,i)
        !将第i个载荷作用的方向赋值给变量m
        m = information_given_displacement(2,i)
        
        !c = 0
        d = 0
        
        !如果节点编号超过要清除的节点号就减去
        do b = 1, amount_node_clear
            if(n .GT. number_node_clear(b)) then
                d = d + 1
            !elseif(j .EQ. number_node_clear(b)) then
            !    j = 1
            endif
        enddo
        n = n - d

        
        !变量mn对应coordinate_nodes_deformed、coordinate_nodes_initial数组中n号节点、m方向的坐标
        mn = 3*(n-1) + m
        
        !将全局刚度阵的第mn行和第mn列置零
        K_global(mn,:) = 0.0
        K_global(:,mn) = 0.0
        !将K_global(mn,mn)设置为1
        K_global(mn,mn) = 1.0
        
        !将force_of_node_cleared(mn)置零
        force_of_node_cleared(mn) = 0.0
        
    enddo
        
end