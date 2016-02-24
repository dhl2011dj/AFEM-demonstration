!****************************************************************************************************
!����ԭ������Ԫ���ӳ������������γɵ�Ԫ�ն���
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
subroutine K_element_assembling(number_element, K_element)
!��Ӧ������ʦ�����е��ӳ���KE

!�������б�����Ӧ����ʦ�����и����ı���
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


!ʹ��afem_parameterģ�飨���ж����˲�����
!��Ԫ�ն��������K_element_row = 6����Ԫ�ն��������K_element_column = 6
use afem_parameter

!Ĭ���������Ҫ��������
implicit none

    !�������
    
    !��Ԫ��ţ�����Ԫѭ���ĵ�Ԫ�ţ������ӳ���K_global_assembling
    integer(kind=4) number_element
    
    !TE
    !��ʱ��֪��������ʲô�ã��ȴ��β����õ�
    !integer(kind=4) TE(number_element)
    !node_number_of_each_element(i)�洢��i����Ԫ�е�amount_node_of_element���ڵ��
    integer(kind=4) node_number_of_each_element(amount_node_of_element)
    
    !��������е�ѭ������
    integer(kind=4) i, j
    
    !�ڵ���������
    !coordinate_nodes_initial(1,I)�����ʼʱ��I���ڵ��X����
    !coordinate_nodes_initial(2,I)�����ʼʱ��I���ڵ��Y����
    !coordinate_nodes_initial(3,I)�����ʼʱ��I���ڵ��Z����
    real(kind=8) coordinate_nodes_initial(3, DOF/3 + amount_node_clear)
    !���κ�ڵ���������
    !coordinate_nodes_deformed(1,I)������κ��I���ڵ��X����
    !coordinate_nodes_deformed(2,I)������κ��I���ڵ��Y����
    !coordinate_nodes_deformed(3,I)������κ��I���ڵ��Z����
    real(kind=8) coordinate_nodes_deformed(3, DOF/3 + amount_node_clear)
    !���ڵĵ�Ԫ������
    !��ʱ���β����õ�
    real(kind=8) current_element_force(K_element_row)
    !��Ԫ�նȾ���K_element_column = 6
    !�о����Ƶ�Ԫ�ն�����ʱ����Ҫ��ô��K_element_row = 6,K_element_column = 6
    !real(kind=8) K_element(30,30)
    real(kind=8) K_element(K_element_row,K_element_column)
    
    !coordinate_node_of_element(i, j)�洢ÿ����Ԫ��i�Žڵ������ֵj = 1, 3�ֱ����i�Žڵ������X��Y��Zֵ
    !amount_node_of_element = 2
    real(kind=8) coordinate_node_of_element(amount_node_of_element,3)
    !��Ԫ��������
    real(kind=8) element_force(K_element_row)
    
    !����ʼֵ
    coordinate_nodes_initial = node_position
    coordinate_nodes_deformed = node_position
    
    write(*,"(/, '*** subroutine - K_element_assembling - begin ***', /)")
    
    !��ʾ��Ԫ���
    write(*,"('*********** ', I10, '�ŵ�Ԫ', '      ***********')") number_element
    
    !��ÿ����Ԫ��i�Žڵ������ֵ��j = 1, 3�ֱ����i�Žڵ������X��Y��Zֵ����������coordinate_node_of_element(i, j)
    !amount_node_of_element = 2
    do i = 1, amount_node_of_element
        !j = 1, 3�����ڵ������X��Y��Zֵ����coordinate_node_of_element(i, j)��
        do j = 1, 3
            coordinate_node_of_element(i, j) = coordinate_nodes_deformed(j, main_node_of_element(i, number_element))
            !�����Ԫ�нڵ��X/Y/Z��������
            write(*,"(1X, 'coordinate_node_of_element(', I3, ',', I3, ')', ' = ', F10.7)") i, j, coordinate_node_of_element(i, j)
        enddo
    enddo
    
    !����number_element����Ԫ�нڵ�Ľڵ�Ÿ�ֵ��node_number_of_each_element����
    node_number_of_each_element(:) = main_node_of_element(:, number_element)
    
    !���ü��㵥Ԫ�ն������������ӳ���
    call calculate_EK_and_EF(coordinate_node_of_element, K_element, element_force)
    
    current_element_force = element_force
    
    write(*,"(/, '**** subroutine - K_element_assembling - end ****', /)")
    
end