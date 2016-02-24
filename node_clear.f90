!****************************************************************************************************
!����ԭ������Ԫ���ӳ��������������û���γɵ�Ԫ�Ľڵ㣺
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
subroutine node_clear()

!ʹ��afem_parameterģ�飨���ж����˲�����
use afem_parameter

!Ĭ���������Ҫ��������
implicit none

    !�������
    
    !�ж��Ƿ�Ҫ������ýڵ�
    integer(kind=4) judge_node_clear
    
    !����ѭ������
    integer(kind=4) i, j, k
    
    !Ϊ����ڵ����������ڴ�ռ�
    allocate(number_node_clear(int(DOF/3)))
    
    !����ʼֵ
    judge_node_clear = 0
    number_node_clear = 0
    !����ڵ�
    !ѭ������ԭ�����
    amount_node_clear = 0
    do k = 1, DOF/3
        
        !����
        judge_node_clear = 0
        
        !��Ԫ�еĵ�j��ԭ��
        do j = 1, amount_node_of_element
            !��Ԫ�нڵ�ŵ�ѭ������i����Ԫ
            do i = 1, amount_all_elements
                
                !���ԭ����Ŵ����ڵ�Ԫ����ɾ��������ɾ��
                if(k .EQ. main_node_of_element(j,i)) then
                    !��Ҫ����ڵ���±�����
                    judge_node_clear = judge_node_clear + 1
                    write(1,"(1X, 'judge_node_clear = ', I3)") judge_node_clear
                    exit
                endif
                
            enddo
            
            !���judge_node_clear��Ϊ0������ѭ��
            if(judge_node_clear .NE. 0) then
                !write(*,"(1X, 'k = ', I3)") k
                exit
            endif
            
        enddo
        
        if(judge_node_clear .EQ. 0) then
            amount_node_clear = amount_node_clear + 1
            !��k��ԭ�ӷ���Ҫ���ԭ�ӵ�������
            number_node_clear(amount_node_clear) = k
        endif
    enddo
    !�������Ҫ������ٸ��ڵ�
    write(*,"(/, 1X, '����Ҫ���', I3, '���ڵ�')") amount_node_clear
    !�����Ҫ�����ԭ��
    write(*,"(1X, '��Ҫ������Ĺ���ԭ�ӣ�')")
    write(*,*)(number_node_clear(i),i = 1,amount_node_clear)
    
    !���
    call output_node_clear()
    
end