!****************************************************************************************************
!这是原子有限元的子程序，作用在于输入求解数据并保存至文件，输入格式和数据如下：
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
subroutine initial_data_input()

!使用afem_parameter模块（其中定义了参数）
use afem_parameter

!默认情况下需要声明变量
implicit none

    !定义变量
    
    !定义x、y、z方向上粒子的个数
    integer(kind=4) number_x, number_y, number_z
    !定义全部粒子的个数和实际存在（去掉中心空洞中的粒子）的粒子的个数
    integer(kind=4) number_total, number_real
    !定义起点的位置
    real(kind=8) center_hole(3)
    !
    real(kind=8) radius_hole!(3)
    !定义位置存储数组（可变数组）
    real(kind=8),allocatable:: position(:,:)
    !定义角度存储数组（可变数组）（研究L-J对势不需要使用）
    !integer(kind=4),allocatable:: angle(:,:)
    real(kind=8) a
    
    !读取参数
    
    !读取x、y、z方向上晶胞的个数
    write(*,*) '请输入x、y、z方向上晶胞的个数(number_x, number_y, number_z)'
    read(*,*) number_x, number_y, number_z
    !每3个原子1个晶胞
    !number_x = 3 * 1
    !number_y = 3 * 1
    !number_z = 3 * 1
    !读取起点的位置
    write(*,*)'请输入起点的位置center(x, y, z)'
    !read(*,*) center_hole(:)
    center_hole(:) = (/0,0,0/)
    !
    write(*,*)'请输入三维圆孔的半径（1个）'!type in Three-dimension cycle radius
    !read(*,*) radius_hole!(3) length_unit_cell = 0.54305
    radius_hole = 0 * length_unit_cell
    
    !计算全部晶胞中粒子的个数，“10”可能代表一个晶胞中粒子的个数
    number_total = number_x * number_y * number_z
    number_real = number_x * number_y * number_z
    
    !给位置存储数组分配内存
    !allocate(pos(3,number_total),stat = error)
    allocate(position(3,number_total))
    
    !即将开始生成节点位置部分的程序
    !write(*,"(/,'1.**************************************************************',/,1X,'开始生成节点位置：',/)")
    write(*,"(1X,'向粒子坐标数组分配空间数number_total = ',I10)") number_total
    
    !调用原子位置生成子程序，进行节点位置生成
    call initial_nodes_form(number_x, number_y, number_z, number_total, number_real, position, center_hole, radius_hole)
    
    !给放置所有节点坐标的全局可变数组node_position配置内存
    allocate(node_position(3, number_real))
    
    !将全局自由度、原子坐标输出
    DOF = 3 * number_real
    write(*,"(/,1X,'全局自由度DOF为：',I10)") DOF
    
    !定义大概的单元数
    !暂时还没有搞清楚单元数目和晶胞数目之间的关系，只能暂时多分配一些
    !根据猜想，单元数目超过每个维度晶胞数目的三次幂，小于四次幂
    amount_all_elements = int(((number_x - 1) * (number_y - 1) * (number_z - 1)) * (number_x - 1)) + 400
    write(*,"(/,1X,'初始分配的全局单元数为：',I10)") amount_all_elements
    
    !将节点坐标传递给全局可变数组node_position
    ![64]allocate(position(3,number_total))
    ![74]allocate(node_position(3,number_real))，node_position中的数据刚好是所有存在的原子的坐标
    !number_total>number_real，number_total - number_real之差为圆孔挖掉的原子坐标的数目
    node_position = position
    
    !输出存储粒子坐标的gro格式文件
    call initial_data_gro_output(number_x, number_y, number_z, number_real, position)
    
    !write(*,"(/,1X,'节点位置生成结束。',/,'1.**************************************************************',/)")
    
end