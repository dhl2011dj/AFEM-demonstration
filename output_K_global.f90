!****************************************************************************************************
!����ԭ������Ԫ���ӳ����������ڱ���ȫ�ָն������ļ��������ʽ���������£�
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
subroutine output_K_global()

!ʹ��afem_parameterģ�飨���ж����˲�����
use afem_parameter

!Ĭ���������Ҫ��������
implicit none

    !�������
    !��ʵ��ȫ�����ɶ�
    integer(kind=4) DOF_global
    
    !����ѭ������
    integer(kind=4) i, j, k
    
    !DOF = DOF - 3*amount_node_clear
    DOF_global = DOF
    
    open(1,file = 'K_global.txt')
    
    write(1,"(1X, 'ȫ�ָնȾ��� K_global = ')")
    do j = 1, DOF_global
        write(1,"(1X, 42F9.3)") (K_global(i,j),i = 1, DOF_global)
    enddo
    
    close(1)
    
end