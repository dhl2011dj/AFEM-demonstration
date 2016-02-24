!****************************************************************************************************
!这是原子有限元的子程序，作用在于搜索所有原子之间的距离判断是否存在原子间（单元），并将单元号和包含的原子号放入全局变量main_node_of_element中，输入格式和数据如下：
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
subroutine element()

!使用afem_parameter模块（其中定义了参数）
use afem_parameter

!默认情况下需要声明变量
implicit none

    !定义变量
    
    !定义节点间距离
    real(kind=8) node_distance_square
    
    !定义距离函数distance的返回值类型
    real(kind=8) distance
    
    !定义距离函数distance_square的返回值类型
    real(kind=8) distance_square
    
    !定义临时存放坐标的两个数组
    real(kind=8) temp_coordinate_i(3)
    real(kind=8) temp_coordinate_j(3)
    
    
    !定义循环变量
    integer(kind=4) i, j, k, m, n
    
    !为包含可供全局使用的单元及其中所包含的原子号的数组分配内存空间
    !main_node_of_element(1, 5)中存储5号单元的第一个节点比如main_node_of_element(1, 5) = 7
    !main_node_of_element(2, 5)中存储5号单元的第一个节点比如main_node_of_element(1, 5) = 8
    allocate(main_node_of_element(amount_node_of_element, amount_all_elements))
    
    
    !赋初始值
    number_node_clear = 0
    !赋予初值
    k = 1
    n = 0
    
    write(*,"(/, '********** subroutine - element - begin ********', /)")
    
    write(*,"(1X,'单元及所包含的原子序号：', /)")
    
    do i = 1, DOF/3
        
        do j = i+1, DOF/3
            !将i号原子和j号原子的坐标分别赋予两个临时数组
            temp_coordinate_i(1:3) = node_position(1:3,i)
            temp_coordinate_j(1:3) = node_position(1:3,j)
            
            !write(*,"(I3, '原子坐标：', 3F10.7)") i, temp_coordinate_i(1:3)
            !write(*,"(I3, '原子坐标：', 3F10.7)") j, temp_coordinate_j(1:3)
            
            node_distance_square = distance_square(temp_coordinate_i, temp_coordinate_j)
            
            !write(*,"(1X, '两原子之间距离node_distance_square = ', F10.6)") node_distance_square
            !write(*,"(1X, '两原子之间截断距离bond_cut_off**2 = ', F10.6)") bond_cut_off**2
            
            !如果两个原子之间距离小于指定截断距离bond_cut_off，则将这两个原子放入一个单元
            if(node_distance_square.LE.bond_cut_off**2) then
                !k号单元中的第1个原子为i号原子
                main_node_of_element(1, k) = i
                !k号单元中的第2个原子为j号原子
                main_node_of_element(2, k) = j
                
                write(*,"(1X, I5,'号单元组成原子：', 2I10)") k, (main_node_of_element(m,k), m = 1,2)
                
                !k值增加，等待下一个单元
                k = k+1
            endif
        enddo
        
    enddo
    
    !write(*,"(1X, '清除节点', I5, '！')") (number_node_clear(i),i=1,n)
    
    !所有单元的个数为k - 1
    amount_all_elements = k - 1
    
    !调用检查孤立原子的子程序
    call node_clear()
    
    write(*,"(/,1X,'实际的的全局单元数为：',I10)") amount_all_elements
    
    write(*,"(/, '********** subroutine - element - end **********', /)")
    
end