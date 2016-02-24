!****************************************************************************************************
!这是原子有限元的模块，原子有限元的各个部分使用的参数、变量、函数在这里声明：
!****************************************************************************************************
module afem_parameter
implicit none

    !定义参数
    
    !单元刚度阵的行数
    integer,parameter::K_element_row = 6
    !单元刚度阵的列数
    integer,parameter::K_element_column = 6
    !单元中节点的个数
    integer,parameter::amount_node_of_element = 2
    
    !real(kind=8) length_unit_cell   
    !real(kind=8) length_cut_off
    !length_unit_cell是晶胞长度
    real,parameter::length_unit_cell = 0.54305
    !length_cut_off是截断距离
    real,parameter::length_cut_off = 0.7 * 0.54305
    !成键的截断距离
    real,parameter::bond_cut_off = 0.8 * 0.54305
    !定义弹簧刚度
    real(kind=8),parameter::stiffness = 3.66666666666
    
    !定义变量
    
    !全局自由度
    integer(kind=4) DOF
    !全部单元数
    integer(kind=4) amount_all_elements
    
    !单元中的原子
    !单元节点编码数组
    !node_of_element(1,I)、node_of_element(2,I)存储第I个单元中的2个节点号
    integer(kind=4),allocatable::main_node_of_element(:,:)
    
    !原子坐标，node_position(1:3,i)表示的是i号原子的X/Y/Z方向坐标
    real(kind=8),allocatable::node_position(:,:)
    
    !需要清除的孤立节点的个数
    integer(kind=4) amount_node_clear
    !存储需要清除的孤立节点的序号
    integer(kind=4),allocatable::number_node_clear(:)
    
    !全局刚度阵
    real(kind=8),allocatable::K_global(:,:)
    !全局力列阵
    real(kind=8),allocatable::force_of_node(:)
    !去除孤立节点后的全局力列阵
    real(kind=8),allocatable::force_of_node_cleared(:)
    
    !边界条件的个数
    integer(kind=4) boundary_conditions
    
    !解矩阵
    real(kind=8),allocatable::X(:)
    
end module