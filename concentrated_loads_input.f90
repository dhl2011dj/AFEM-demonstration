!****************************************************************************************************
!这是原子有限元的子程序，作用在于输入集中载荷数据并保存至文件，输入格式和数据如下：
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
subroutine concentrated_loads_input(number_force)

!使用afem_parameter模块（其中定义了参数）
use afem_parameter

!本程序中变量对应刘老师代码中给出的变量
!number_force       =>  NF
!information_force  =>  MF
!value_force        =>  ZF

!默认情况下需要声明变量
implicit none

    !定义变量
    
    !作用于节点上的集中载荷（坐标方向）的个数
    integer(kind=4) number_force
    
    !定义循环变量
    integer(kind=4) i, j, k
    
    !作用于节点上的集中载荷的信息数组
    !information_force(1,i)：第i个集中载荷作用的节点号
    !information_force(2,i) = 1：作用X方向的集中力
    !information_force(2,i) = 2：作用Y方向的集中力
    !information_force(2,i) = 3：作用Y方向的集中力
    integer(kind=4) information_force(2, number_force)
    
    !作用集中力的值（以坐标方向为正）
    real(kind=8) value_force(number_force)
    
    !赋初始值
    information_force(1,:) = 0
    information_force(2,:) = 0
    
    write(*,"(1X, '需要清理的节点数 = ', I5)") amount_node_clear
    
    write(*,"(/, 1X, '************ 开始输入集中载荷 ************')")
    !读取存有集中载荷个数、信息和值的文件
    do i = 1, number_force
        write(*,"('请输入information_force(1,i)：第i个集中载荷作用的节点号')")
        read(*,*) information_force(1,i)
        !判断information_force(1,i)中的数值是否合理
        do while((information_force(1,i).LE.0).OR.(information_force(1,i).GE.(DOF + 3*amount_node_clear)/3+1))
            
            write(*,"(1X, '作用力所在原子输入有误，请重新输入。')")
            information_force(1,i) = 0
            read(*,*) information_force(1,i)
            
        enddo
        
        do while(.true.)
        
        !判断原子是否为孤立原子
            do k = 1, amount_node_clear
                
                if(information_force(1,i) .EQ. number_node_clear(k)) then
                    
                    write(*,"(1X, '作用力所在原子为孤立原子，请重新输入。')")
                    information_force(1,i) = 0
                    read(*,*) information_force(1,i)
                    
                endif
                
            enddo
            
            !退出无限循环
            exit
            
        enddo
        
        write(*,"('请输入information_force(2,i)：', /, ' = 1代表X方向作用的集中力', /, ' = 2代表Y方向作用的集中力', /, ' = 3代表Z方向作用的集中力')")
        read(*,*) information_force(2,i)
        !判断information_force(2,i)中的数值是否合理
        do while((information_force(2,i).NE.1).AND.(information_force(2,i).NE.2).AND.(information_force(2,i).NE.3))
            
            write(*,"(1X, '作用力方向输入有误，请重新输入。')")
            !write(*,"(' = 1代表X方向作用的集中力', /, ' = 2代表Y方向作用的集中力', /, ' = 3代表Z方向作用的集中力')")
            information_force(2,i) = 0
            read(*,*) information_force(2,i)
            
        enddo
        
        write(*,"('请输入value_force(i)：第i个集中力的大小')")
        read(*,*) value_force(i)
        
        !read(unit=5,fmt=*)代表从键盘输入
        !read(5,*)(information_force(j,i), j = 1,2), value_force(i)
    enddo
    
    write(*,"(1X, '************ 集中载荷输入结束 ************', /)")
    
    write(*,"(/, 1X, '********** 集中载荷开始写入文件 **********')")
    open(1,file='concentrated_loads.txt')
    
    !输出字符串'34HCONCENTRATED LOADS information_force(3,NF) value_force(NF)'
    write(1,"(1X, 'concentrated loads information_force(3,number_force) value_force(number_force)')")
    
    !输出所有集中力对应的：
    !information_force(1,i)、information_force(2,i)、value_force(i)
    !information_force(1,i)：第i个集中载荷作用的节点号
    !information_force(2,i) = 1：作用X方向的集中力
    !information_force(2,i) = 2：作用Y方向的集中力
    !information_force(2,i) = 3：作用Y方向的集中力
    do i = 1, number_force
        write(1,"(/,2I10,F20.6)") (information_force(j,i),j=1,2), value_force(i)
    enddo
    
    close(1)
    write(*,"(/, 1X, '- \ | / - \ | / - \ | / - \ | / - \ | /')")
    write(*,"(/, 1X, '********** 集中载荷写入文件结束 **********')")
    
    !调用子程序将集中载荷数据组装进入全局力列阵
    call concentrated_loads_assembling(number_force, information_force, value_force)
    
end