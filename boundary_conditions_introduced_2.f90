!****************************************************************************************************
!这是原子有限元的子程序，作用在于利用边界条件（给定位移值）消除总刚奇异性，输入格式和数据如下：
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
subroutine boundary_conditions_introduced_2(number_given_displacement)

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
    integer(kind=4) information_given_displacement(2, number_given_displacement)
    
    !
    integer(kind=4) S
    !定义循环变量
    integer(kind=4) i, j
    !变量m存储第i个载荷作用的方向，变量n存储第i个载荷作用的节点号
    integer(kind=4) m, n
    !变量mn存储载荷对应节点、对应方向的坐标
    !变量mn对应coordinate_nodes_deformed、coordinate_nodes_initial数组中n号节点、m方向的坐标
    integer(kind=4) mn

    
    !给定位移边界条件的值
    !
    real(kind=8) value_given_displacement(number_given_displacement)
    !
    real(kind=8) Z
    
    !按二维等宽数组存储的刚度阵
    real(kind=8) K_global_bandwidth(DOF, DOF)
    
    !初始节点坐标
    real(kind=8) coordinate_nodes_initial(DOF)
    !变形后节点坐标
    real(kind=8) coordinate_nodes_deformed(DOF)
    
    !
    real(kind=8) FORCE(DOF)
    !仅仅是force_of_node在本子程序中的副本
    real(kind=8) FACTOR
    !
    real(kind=8) FF(DOF)
    !
    real(kind=8) ERRF
    
    !变量初始化，将force_of_node赋值给FORCE
    FORCE = force_of_node
    !变量初始化
    FF = 1.0
    
    !第一层循环：
    !1==============================================================================
    do i = 1, number_given_displacement
        !将第i个载荷作用的节点号赋值给变量n
        n = information_given_displacement(1,i)
        !将第i个载荷作用的方向赋值给变量m
        m = information_given_displacement(2,i)
        !变量mn对应coordinate_nodes_deformed、coordinate_nodes_initial数组中n号节点、m方向的坐标
        mn = 3*(n-1) + m
        
        Z = (value_given_displacement(i) * FACTOR - (coordinate_nodes_deformed(mn) - coordinate_nodes_initial(mn)))
        
        S = 0
        FORCE(mn) = 0.0
        FF(mn) = 0.0
        
        !第二层循环：
        !2==========================================================================
        do j = 2, DOF
            !dabs是双精度变量取绝对值的函数
            S = S + dabs(K_global_bandwidth(mn, j))
            if(mn + 1 - j.GT.0) then
                S = S + dabs(K_global_bandwidth(mn + 1 - j, j))
            endif
        enddo
        !2==========================================================================
        !K_global_bandwidth(mn, 1)变成一个超级大的数
        K_global_bandwidth(mn, 1) = S*1.E+15+1
        force_of_node(mn) = K_global_bandwidth(mn, 1) * Z
    enddo
    !1==============================================================================
    ERRF = 0.0
    do i = i, DOF
        if(ERRF.LT.dabs(FORCE(i))) then
            ERRF = dabs(FORCE(i))
        endif
    enddo
    write(*,"(1X,'最大梯度力：MAX RESIDUAL FORCE = ')")
end