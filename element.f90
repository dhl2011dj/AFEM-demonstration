!****************************************************************************************************
!����ԭ������Ԫ���ӳ�������������������ԭ��֮��ľ����ж��Ƿ����ԭ�Ӽ䣨��Ԫ����������Ԫ�źͰ�����ԭ�Ӻŷ���ȫ�ֱ���main_node_of_element�У������ʽ���������£�
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
subroutine element()

!ʹ��afem_parameterģ�飨���ж����˲�����
use afem_parameter

!Ĭ���������Ҫ��������
implicit none

    !�������
    
    !����ڵ�����
    real(kind=8) node_distance_square
    
    !������뺯��distance�ķ���ֵ����
    real(kind=8) distance
    
    !������뺯��distance_square�ķ���ֵ����
    real(kind=8) distance_square
    
    !������ʱ����������������
    real(kind=8) temp_coordinate_i(3)
    real(kind=8) temp_coordinate_j(3)
    
    
    !����ѭ������
    integer(kind=4) i, j, k, m, n
    
    !Ϊ�����ɹ�ȫ��ʹ�õĵ�Ԫ��������������ԭ�Ӻŵ���������ڴ�ռ�
    !main_node_of_element(1, 5)�д洢5�ŵ�Ԫ�ĵ�һ���ڵ����main_node_of_element(1, 5) = 7
    !main_node_of_element(2, 5)�д洢5�ŵ�Ԫ�ĵ�һ���ڵ����main_node_of_element(1, 5) = 8
    allocate(main_node_of_element(amount_node_of_element, amount_all_elements))
    
    
    !����ʼֵ
    number_node_clear = 0
    !�����ֵ
    k = 1
    n = 0
    
    write(*,"(/, '********** subroutine - element - begin ********', /)")
    
    write(*,"(1X,'��Ԫ����������ԭ����ţ�', /)")
    
    do i = 1, DOF/3
        
        do j = i+1, DOF/3
            !��i��ԭ�Ӻ�j��ԭ�ӵ�����ֱ���������ʱ����
            temp_coordinate_i(1:3) = node_position(1:3,i)
            temp_coordinate_j(1:3) = node_position(1:3,j)
            
            !write(*,"(I3, 'ԭ�����꣺', 3F10.7)") i, temp_coordinate_i(1:3)
            !write(*,"(I3, 'ԭ�����꣺', 3F10.7)") j, temp_coordinate_j(1:3)
            
            node_distance_square = distance_square(temp_coordinate_i, temp_coordinate_j)
            
            !write(*,"(1X, '��ԭ��֮�����node_distance_square = ', F10.6)") node_distance_square
            !write(*,"(1X, '��ԭ��֮��ضϾ���bond_cut_off**2 = ', F10.6)") bond_cut_off**2
            
            !�������ԭ��֮�����С��ָ���ضϾ���bond_cut_off����������ԭ�ӷ���һ����Ԫ
            if(node_distance_square.LE.bond_cut_off**2) then
                !k�ŵ�Ԫ�еĵ�1��ԭ��Ϊi��ԭ��
                main_node_of_element(1, k) = i
                !k�ŵ�Ԫ�еĵ�2��ԭ��Ϊj��ԭ��
                main_node_of_element(2, k) = j
                
                write(*,"(1X, I5,'�ŵ�Ԫ���ԭ�ӣ�', 2I10)") k, (main_node_of_element(m,k), m = 1,2)
                
                !kֵ���ӣ��ȴ���һ����Ԫ
                k = k+1
            endif
        enddo
        
    enddo
    
    !write(*,"(1X, '����ڵ�', I5, '��')") (number_node_clear(i),i=1,n)
    
    !���е�Ԫ�ĸ���Ϊk - 1
    amount_all_elements = k - 1
    
    !���ü�����ԭ�ӵ��ӳ���
    call node_clear()
    
    write(*,"(/,1X,'ʵ�ʵĵ�ȫ�ֵ�Ԫ��Ϊ��',I10)") amount_all_elements
    
    write(*,"(/, '********** subroutine - element - end **********', /)")
    
end