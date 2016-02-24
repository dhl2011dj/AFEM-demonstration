!****************************************************************************************************
!����ԭ������Ԫ���ӳ���������������������ݲ��������ļ��������ʽ���������£�
!
!���������input����
!
!���������output����
!
!�½�������new����
!
!�����ӳ��򣨺�������
!
!�������ӳ��򣨺�������
!
!��д�ļ�����
!
!****************************************************************************************************
subroutine initial_data_input()

!ʹ��afem_parameterģ�飨���ж����˲�����
use afem_parameter

!Ĭ���������Ҫ��������
implicit none

    !�������
    
    !����x��y��z���������ӵĸ���
    integer(kind=4) number_x, number_y, number_z
    !����ȫ�����ӵĸ�����ʵ�ʴ��ڣ�ȥ�����Ŀն��е����ӣ������ӵĸ���
    integer(kind=4) number_total, number_real
    !��������λ��
    real(kind=8) center_hole(3)
    !
    real(kind=8) radius_hole!(3)
    !����λ�ô洢���飨�ɱ����飩
    real(kind=8),allocatable:: position(:,:)
    !����Ƕȴ洢���飨�ɱ����飩���о�L-J���Ʋ���Ҫʹ�ã�
    !integer(kind=4),allocatable:: angle(:,:)
    real(kind=8) a
    
    !��ȡ����
    
    !��ȡx��y��z�����Ͼ����ĸ���
    write(*,*) '������x��y��z�����Ͼ����ĸ���(number_x, number_y, number_z)'
    read(*,*) number_x, number_y, number_z
    !ÿ3��ԭ��1������
    !number_x = 3 * 1
    !number_y = 3 * 1
    !number_z = 3 * 1
    !��ȡ����λ��
    write(*,*)'����������λ��center(x, y, z)'
    !read(*,*) center_hole(:)
    center_hole(:) = (/0,0,0/)
    !
    write(*,*)'��������άԲ�׵İ뾶��1����'!type in Three-dimension cycle radius
    !read(*,*) radius_hole!(3) length_unit_cell = 0.54305
    radius_hole = 0 * length_unit_cell
    
    !����ȫ�����������ӵĸ�������10�����ܴ���һ�����������ӵĸ���
    number_total = number_x * number_y * number_z
    number_real = number_x * number_y * number_z
    
    !��λ�ô洢��������ڴ�
    !allocate(pos(3,number_total),stat = error)
    allocate(position(3,number_total))
    
    !������ʼ���ɽڵ�λ�ò��ֵĳ���
    !write(*,"(/,'1.**************************************************************',/,1X,'��ʼ���ɽڵ�λ�ã�',/)")
    write(*,"(1X,'�����������������ռ���number_total = ',I10)") number_total
    
    !����ԭ��λ�������ӳ��򣬽��нڵ�λ������
    call initial_nodes_form(number_x, number_y, number_z, number_total, number_real, position, center_hole, radius_hole)
    
    !���������нڵ������ȫ�ֿɱ�����node_position�����ڴ�
    allocate(node_position(3, number_real))
    
    !��ȫ�����ɶȡ�ԭ���������
    DOF = 3 * number_real
    write(*,"(/,1X,'ȫ�����ɶ�DOFΪ��',I10)") DOF
    
    !�����ŵĵ�Ԫ��
    !��ʱ��û�и������Ԫ��Ŀ�;�����Ŀ֮��Ĺ�ϵ��ֻ����ʱ�����һЩ
    !���ݲ��룬��Ԫ��Ŀ����ÿ��ά�Ⱦ�����Ŀ�������ݣ�С���Ĵ���
    amount_all_elements = int(((number_x - 1) * (number_y - 1) * (number_z - 1)) * (number_x - 1)) + 400
    write(*,"(/,1X,'��ʼ�����ȫ�ֵ�Ԫ��Ϊ��',I10)") amount_all_elements
    
    !���ڵ����괫�ݸ�ȫ�ֿɱ�����node_position
    ![64]allocate(position(3,number_total))
    ![74]allocate(node_position(3,number_real))��node_position�е����ݸպ������д��ڵ�ԭ�ӵ�����
    !number_total>number_real��number_total - number_real֮��ΪԲ���ڵ���ԭ���������Ŀ
    node_position = position
    
    !����洢���������gro��ʽ�ļ�
    call initial_data_gro_output(number_x, number_y, number_z, number_real, position)
    
    !write(*,"(/,1X,'�ڵ�λ�����ɽ�����',/,'1.**************************************************************',/)")
    
end