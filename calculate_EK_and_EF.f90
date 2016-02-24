!****************************************************************************************************
!����ԭ������Ԫ���ӳ����������ڼ��㵥Ԫ�ն������������ӳ���
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
!subroutine calculate_EK_and_EF(coordinate_node_of_element, amount_node_of_element, K_element, element_force)
subroutine calculate_EK_and_EF(coordinate_node_of_element, K_element, element_force)

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

!�������У�
!����Ҫ��EX���X����ˣ�
!coordinate_node_of_element =>  EX  =>  X
!����Ҫ��NELE���NSE����ˣ�
!node_number_of_each_element=>  NELE=>  NSE


!ʹ��afem_parameterģ�飨���ж����˲�����
use afem_parameter


!Ĭ���������Ҫ��������
implicit none

    !�������
    
    !node_number_of_each_element(i)�洢��i����Ԫ�е�amount_node_of_element���ڵ��
    integer(kind=4)node_number_of_each_element(amount_node_of_element)
    
    !coordinate_node_of_element(i, j)�洢ÿ����Ԫ��i�Žڵ������ֵj = 1, 3�ֱ����i�Žڵ������X��Y��Zֵ
    !amount_node_of_element = 2
    real(kind=8) coordinate_node_of_element(amount_node_of_element,3)
    !��Ԫ��������K_element_row = 6
    real(kind=8) element_force(K_element_row)
    !��Ԫ�նȾ���K_element_column = 6
    !�о����Ƶ�Ԫ�ն�����ʱ����Ҫ��ô��K_element_row = 6,K_element_column = 6
    !real(kind=8) K_element(30,30)
    real(kind=8) K_element(K_element_row,K_element_column)
    
    !����ѭ������
    integer(kind=4) i, j, m
    
    K_element = 0.0
    element_force = 0.0
    
    
    !���µ�Ԫ������F + ?u/?x
    do m =1, 3
        element_force(m) = element_force(m) + stiffness * (dabs(coordinate_node_of_element(1, m) - coordinate_node_of_element(2, m)))
    enddo
    
    !�γɵ�Ԫ�ն���
    !mΪ��(3*(i - 1) + j)�ศ���ı��������ڸ���Ԫ�ն���ĶԳ�λ�ø�ֵ
    !m�����ֵӦ����3����amount_node_of_element = 2
    do m = 1, 3 * amount_node_of_element
        !����Ԫ�еĽڵ��ѭ��
        do i = 1, amount_node_of_element
            !������ûʲô��
            !if((i.EQ.1).OR.(node_number_of_each_element(i).NE.node_number_of_each_element(1))) then
                !��X��Y��Z��������ѭ��
                do j = 1, 3
                    !���������������е�Ԫ�أ����öԳ��γ��������ǵ�Ԫ��
                    if(m.GE.(3*(i - 1) + j)) then
                        !����γɵ��ǵ�Ԫ��1�Žڵ㣨��Ԫ����ţ�����ڵ�նȼ�������u��λ��x�Ķ���ƫ������?2u/?x1?x1
                        if (m.EQ.(3*(i - 1) + j)) then
                            K_element(m,(3*(i - 1) + j)) = K_element(m,(3*(i - 1) + j)) + stiffness
                        !����γɵ��ǵ�Ԫ��2�Žڵ㣨��Ԫ����ţ������ƣ�����ڵ�նȼ�������u��λ��x�Ķ���ƫ������?2u/?x?x
                        else if((m-3).EQ.(3*(i - 1) + j)) then
                            K_element(m,(3*(i - 1) + j)) = K_element(m,(3*(i - 1) + j)) - stiffness
                        endif
                        !��Ԫ�ն���Ϊ�Գ��󣬸��Գ�λ��Ԫ�ظ�ֵ
                        K_element((3*(i - 1) + j),m) = K_element(m,(3*(i - 1) + j))
                    else
                        !����ѭ��
                        cycle
                    endif
                enddo
                !��X��Y��Z��������ѭ������
            !endif
        enddo
        !����Ԫ�еĽڵ��ѭ������
    enddo
    !�γɵ�Ԫ�ն������
    !write(*,"('��Ԫ�նȾ��� K_element = ')")
    do j = 1, K_element_column
        !write(*,"(1X, 6F9.3)") (K_element(i,j),i = 1, K_element_row)
    enddo

end