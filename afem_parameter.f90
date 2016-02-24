!****************************************************************************************************
!����ԭ������Ԫ��ģ�飬ԭ������Ԫ�ĸ�������ʹ�õĲ���������������������������
!****************************************************************************************************
module afem_parameter
implicit none

    !�������
    
    !��Ԫ�ն��������
    integer,parameter::K_element_row = 6
    !��Ԫ�ն��������
    integer,parameter::K_element_column = 6
    !��Ԫ�нڵ�ĸ���
    integer,parameter::amount_node_of_element = 2
    
    !real(kind=8) length_unit_cell   
    !real(kind=8) length_cut_off
    !length_unit_cell�Ǿ�������
    real,parameter::length_unit_cell = 0.54305
    !length_cut_off�ǽضϾ���
    real,parameter::length_cut_off = 0.7 * 0.54305
    !�ɼ��ĽضϾ���
    real,parameter::bond_cut_off = 0.8 * 0.54305
    !���嵯�ɸն�
    real(kind=8),parameter::stiffness = 3.66666666666
    
    !�������
    
    !ȫ�����ɶ�
    integer(kind=4) DOF
    !ȫ����Ԫ��
    integer(kind=4) amount_all_elements
    
    !��Ԫ�е�ԭ��
    !��Ԫ�ڵ��������
    !node_of_element(1,I)��node_of_element(2,I)�洢��I����Ԫ�е�2���ڵ��
    integer(kind=4),allocatable::main_node_of_element(:,:)
    
    !ԭ�����꣬node_position(1:3,i)��ʾ����i��ԭ�ӵ�X/Y/Z��������
    real(kind=8),allocatable::node_position(:,:)
    
    !��Ҫ����Ĺ����ڵ�ĸ���
    integer(kind=4) amount_node_clear
    !�洢��Ҫ����Ĺ����ڵ�����
    integer(kind=4),allocatable::number_node_clear(:)
    
    !ȫ�ָն���
    real(kind=8),allocatable::K_global(:,:)
    !ȫ��������
    real(kind=8),allocatable::force_of_node(:)
    !ȥ�������ڵ���ȫ��������
    real(kind=8),allocatable::force_of_node_cleared(:)
    
    !�߽������ĸ���
    integer(kind=4) boundary_conditions
    
    !�����
    real(kind=8),allocatable::X(:)
    
end module