!****************************************************************************************************
!这是原子有限元的子程序，作用在于生成C、Si原子位置数据并保存至文件，输入格式和数据如下：
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
subroutine initial_nodes_form(number_x, number_y, number_z, number_total, number_real, position, center_hole, radius_hole)

!使用afem_parameter模块（其中定义了参数）length_unit_cell、length_cut_off
use afem_parameter

!默认情况下需要声明变量
implicit none

    !定义变量
    
    !定义x、y、z方向上晶胞的个数
    integer(kind=4),intent(in)::number_x, number_y, number_z
    !定义全部晶胞的个数
    integer(kind=4),intent(in)::number_total
    !定义实际存在的晶胞的个数
    integer(kind=4),intent(out)::number_real
    !定义孔的中心和半径
    real(kind=8),intent(in)::center_hole(3), radius_hole
    !定义位置存储数组
    !position(1,:)代表所有节点的X坐标
    !position(2,:)代表所有节点的Y坐标
    !position(3,:)代表所有节点的Z坐标
    real(kind=8)::position(3,number_total)
    
    !定义角度存储数组（可变数组）（研究L-J对势不需要使用）
    !integer(kind=4),allocatable:: angle(:,:)

    !所有粒子坐标中心
    !real(kind=8),intent(in)::center_all_nodes(3)
    real(kind=8) center_all_nodes(3)
    !定义x、y、z方向上的序号
    integer(kind=4) i_x, i_y, i_z
    !
    integer(kind=4) label_x, label_y, label_z
    !所有粒子坐标总和
    real(kind=8) total_xyz_all_nodes(3)
    !循环变量
    integer(kind=4) i, j
    !计算两点之间距离的函数
    real(kind=8),external::distance
    !在清除孔内节点坐标时，粒子距离孔圆心的距离
    real(kind=8) r
    !需要清除（覆盖）的节点号
    real(kind=8) node_clear
    !real(kind=8) length_unit_cell = 0.54305    
    label_z = 0
    
    !进行循环生成粒子位置
    !第一层循环：i_x、i_y为奇数或偶数对应着两种Z方向上不同的粒子分布方式
    !1==============================================================================
    do i_x = 1, number_x
        !本层循环中label_x都是相同的值
        label_x = label_z
        
        !如果i_x为偶数和i_y为奇数或偶数时对应着两种Z方向上不同的粒子分布方式
        if(mod(i_x, 2) == 0) then
            
            !第二层循环：i_y为偶数,那么Z方向在奇数位置会出现粒子
            !2======================================================================
            do i_y = 1, number_y
                !本层循环中每个相同的label_y对应着一个循环的label_z
                label_y = label_z
                
                !如果i_y为偶数,那么Z方向在奇数位置会出现粒子
                if(mod(i_y, 2) == 0) then
                    
                    !第三层循环：Z方向在奇数位置会出现粒子
                    !3==============================================================
                    do i_z = 1, number_z
                        if(mod(i_z, 2) /= 0) then
                            !每存入一个粒子的位置，label_z增加一个
                            label_z = label_z + 1
                            !将粒子的Z坐标存入position(3,:)数组
                            position(3, label_z) = length_unit_cell / 2.0 * dble(i_z-1)
                        end if
                    end do
                    !3==============================================================
                    
                !如果i_y为奇数,那么Z方向在偶数位置会出现粒子
                else if(mod(i_y, 2) /= 0) then
                    
                    !第三层循环：Z方向在偶数位置会出现粒子
                    !3==============================================================
                    do i_z = 1, number_z
                        if(mod(i_z, 2) == 0) then
                            !每存入一个粒子的位置，label_z增加一个
                            label_z = label_z + 1
                            !将粒子的Z坐标存入position(3,:)数组
                            position(3, label_z) = length_unit_cell / 2.0 * dble(i_z-1)
                        end if
                    end do
                    !3==============================================================
                    
                end if
                !利用label_y - label_z对每组label_y相同的粒子进行批量赋值
                position(2, label_y+1:label_z) = length_unit_cell / 2 * dble(i_y-1)
            end do
            !2======================================================================
            
        !如果i_x为奇数和i_y为奇数或偶数时对应着两种Z方向上不同的粒子分布方式
        else if(mod(i_x, 2) /= 0) then
            
            !第二层循环：i_y为偶数,那么Z方向在偶数位置会出现粒子
            !2======================================================================
            do i_y = 1, number_y
                !本层循环中每个相同的label_y对应着一个循环的label_z
                label_y = label_z
                
                !如果i_y为偶数,那么Z方向在偶数位置会出现粒子
                if(mod(i_y, 2) == 0) then
                    
                    !第三层循环：Z方向在偶数位置会出现粒子
                    !3==============================================================
                    do i_z = 1, number_z
                        if(mod(i_z, 2) == 0) then
                            !每存入一个粒子的位置，label_z增加一个
                            label_z = label_z + 1
                            !将粒子的Z坐标存入position(3,:)数组
                            position(3, label_z) = length_unit_cell / 2.0 * dble(i_z-1)
                        end if
                    end do
                    !3==============================================================
                
                !如果i_y为奇数,那么Z方向在奇数位置会出现粒子
                else if(mod(i_y, 2) /= 0) then
                    
                    !第三层循环：Z方向在奇数位置会出现粒子
                    !3==============================================================
                    do i_z = 1, number_z
                        if(mod(i_z, 2) /= 0) then
                            !每存入一个粒子的位置，label_z增加一个
                            label_z = label_z + 1
                            !将粒子的Z坐标存入position(3,:)数组
                            position(3, label_z) = length_unit_cell / 2.0 * dble(i_z-1)
                        end if
                    end do
                    !3==============================================================
                    
                end if
                !利用label_y - label_z对每组label_y相同的粒子进行批量赋值
                position(2, label_y+1 : label_z) = length_unit_cell / 2 * dble(i_y-1)
            end do
            !2======================================================================
            
        end if
        !利用label_x - label_z对每组label_x相同的粒子进行批量赋值
        position(1, label_x+1 : label_z) = length_unit_cell / 2 * dble(i_x-1)
        
    !*******************************************************************************
        if(i_x < number_x) then
            label_x = label_z
            !如果i_x为偶数和i_y为奇数或偶数时对应着两种Z方向上不同的粒子分布方式
            if(mod(i_x, 2) == 0) then
            
                !第二层循环：i_y为偶数,那么Z方向在奇数位置会出现粒子
                !2======================================================================
                do i_y = 1, number_y - 1
                    !本层循环中每个相同的label_y对应着一个循环的label_z
                    label_y = label_z
                
                    !如果i_y为偶数,那么Z方向在奇数位置会出现粒子
                    if(mod(i_y, 2) == 0) then
                    
                        !第三层循环：Z方向在奇数位置会出现粒子
                        !3==============================================================
                        do i_z = 1, number_z - 1
                            if(mod(i_z, 2) /= 0) then
                                !每存入一个粒子的位置，label_z增加一个
                                label_z = label_z + 1
                                !将粒子的Z坐标存入position(3,:)数组
                                position(3, label_z) = length_unit_cell / 2.0 * dble(i_z-1) + length_unit_cell / 4.0
                            end if
                        end do
                        !3==============================================================
                    
                    !如果i_y为奇数,那么Z方向在偶数位置会出现粒子
                    else if(mod(i_y, 2) /= 0) then
                    
                        !第三层循环：Z方向在偶数位置会出现粒子
                        !3==============================================================
                        do i_z = 1, number_z - 1
                            if(mod(i_z, 2) == 0) then
                                !每存入一个粒子的位置，label_z增加一个
                                label_z = label_z + 1
                                !将粒子的Z坐标存入position(3,:)数组
                                position(3, label_z) = length_unit_cell / 2.0 * dble(i_z-1) + length_unit_cell / 4.0
                            end if
                        end do
                        !3==============================================================
                    
                    end if
                    !利用label_y - label_z对每组label_y相同的粒子进行批量赋值
                    position(2, label_y+1:label_z) = length_unit_cell / 2 * dble(i_y-1) + length_unit_cell / 4.0
                end do
                !2======================================================================
            
            !如果i_x为奇数和i_y为奇数或偶数时对应着两种Z方向上不同的粒子分布方式
            else if(mod(i_x, 2) /= 0) then
            
                !第二层循环：i_y为偶数,那么Z方向在偶数位置会出现粒子
                !2======================================================================
                do i_y = 1, number_y - 1
                    !本层循环中每个相同的label_y对应着一个循环的label_z
                    label_y = label_z
                
                    !如果i_y为偶数,那么Z方向在偶数位置会出现粒子
                    if(mod(i_y, 2) == 0) then
                    
                        !第三层循环：Z方向在偶数位置会出现粒子
                        !3==============================================================
                        do i_z = 1, number_z - 1
                            if(mod(i_z, 2) == 0) then
                                !每存入一个粒子的位置，label_z增加一个
                                label_z = label_z + 1
                                !将粒子的Z坐标存入position(3,:)数组
                                position(3, label_z) = length_unit_cell / 2.0 * dble(i_z-1) + length_unit_cell / 4.0
                            end if
                        end do
                        !3==============================================================
                
                    !如果i_y为奇数,那么Z方向在奇数位置会出现粒子
                    else if(mod(i_y, 2) /= 0) then
                    
                        !第三层循环：Z方向在奇数位置会出现粒子
                        !3==============================================================
                        do i_z = 1, number_z - 1
                            if(mod(i_z, 2) /= 0) then
                                !每存入一个粒子的位置，label_z增加一个
                                label_z = label_z + 1
                                !将粒子的Z坐标存入position(3,:)数组
                                position(3, label_z) = length_unit_cell / 2.0 * dble(i_z-1) + length_unit_cell / 4.0
                            end if
                        end do
                        !3==============================================================
                    
                    end if
                    !利用label_y - label_z对每组label_y相同的粒子进行批量赋值
                    position(2, label_y+1 : label_z) = length_unit_cell / 2 * dble(i_y-1) + length_unit_cell / 4.0
                end do
                !2======================================================================
            
            end if
            !利用label_x - label_z对每组label_x相同的粒子进行批量赋值
            position(1, label_x+1 : label_z) = length_unit_cell / 2 * dble(i_x-1) + length_unit_cell / 4.0

        end if
    
    end do
    !1============================================================================== 
    
    
    !利用前面的label_z得到实际的粒子数
    number_real = label_z
    write(*,"(1X,'去掉中心圆孔中的粒子前，粒子数number_real = ',I10)") number_real
    !write(*,*) position(:,1:number_real)
    !坐标总和置零
    total_xyz_all_nodes(1) = 0.0
    total_xyz_all_nodes(2) = 0.0
    total_xyz_all_nodes(3) = 0.0
    
    !将所有粒子的X、Y、Z坐标相加求中心位置
    do i = 1, number_real
        total_xyz_all_nodes(1) = total_xyz_all_nodes(1) + position(1,i)
        total_xyz_all_nodes(2) = total_xyz_all_nodes(2) + position(2,i)
        total_xyz_all_nodes(3) = total_xyz_all_nodes(3) + position(3,i)
    end do
    center_all_nodes(1) = total_xyz_all_nodes(1) / dble(number_real)
    center_all_nodes(2) = total_xyz_all_nodes(2) / dble(number_real)
    center_all_nodes(3) = total_xyz_all_nodes(3) / dble(number_real)
    
    !对所有坐标进行重置，坐标原点设置在中心
    !do i = 1, number_real
    !    position(1,i) = position(1,i) - center_all_nodes(1)
    !    position(2,i) = position(2,i) - center_all_nodes(2)
    !    position(3,i) = position(3,i) - center_all_nodes(3)
    !end do
    
    
    !去除孔内的节点
    !如果粒子坐标在孔外的话，i按顺序增加而c没有增加
    !然后将粒子i的坐标向前覆盖粒子c的坐标
    node_clear = 0
    do i = 1, number_real
        r = distance(position(:,i), center_hole)
        if(r >= radius_hole) then
            node_clear = node_clear + 1
            !将后面孔外的粒子坐标向前覆盖
            do j = 1, 3
                position(j, node_clear)=position(j, i)
            enddo
        endif
    end do
    
    !将孔外所有粒子的个数赋给number_real
    number_real = node_clear
    write(*,"(1X,'去掉中心圆孔中的粒子后，粒子数number_real = ',I10)") number_real
    
end