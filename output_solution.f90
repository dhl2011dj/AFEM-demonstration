subroutine output_solution()

!ʹ��afem_parameterģ�飨���ж����˲�����
use afem_parameter

!Ĭ���������Ҫ��������
implicit none

    !�������
    
    !����ѭ������
    integer(kind=4) i, j, k
    
    !�����߼�����
    logical judge_node_clear
    !����ʼֵ
    judge_node_clear = .true.
    !k�ǽ�force_of_node�е���Ч�ڵ�������orce_of_node_clearedʱ��ѭ������
    k = 0
    
    
    open(1,file = 'X.txt')
    
    write(1,"(1X, 'ԭ��λ�ƽ����� X = ')")
    do i = 1, DOF + 3*amount_node_clear
        
        !���ñ���
        judge_node_clear = .true.
        
        !�����force_of_node��ʽ
        !write(1,"(1X, F9.3)") force_of_node(i)
        
        !�ж�ԭ���Ƿ�Ϊ��ɢԭ��
        do j = 1, amount_node_clear
            if((int((i-1)/3)+1) .EQ. number_node_clear(j)) then
                judge_node_clear = .false.
            endif
        enddo
        
        !�����ϸ�ѭ��������judge_node_clear���ж��Ƿ����
        if(judge_node_clear) then
            
            k = k + 1
            !��ԭ����źͷ�����ŵ������ʽ
            write(*,"(1X, I5, 'ԭ��', I2, '����λ�ƣ�', F9.3)") (int((i-1)/3)+1), (i-3*int((i-1)/3)), X(k)
            write(1,"(1X, I5, 'ԭ��', I2, '����λ�ƣ�', F9.3)") (int((i-1)/3)+1), (i-3*int((i-1)/3)), X(k)
            
        endif
    enddo
    
    close(1)
    
end