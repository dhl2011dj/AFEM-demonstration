!****************************************************************************************************
!����ԭ������Ԫ���ӳ������������������غ����ݵĵ�Ч�ڵ���������Ч�ڵ��������������������ʽ���������£�
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
subroutine distributed_loads_assembling(number_pressure, number_row_K_global)

!�������б�����Ӧ����ʦ�����и����ı���
!number_pressure       =>  NP
!information_pressure  =>  MP
!value_pressure        =>  ZP

!Ĭ���������Ҫ��������
implicit none

    !�������
    
    !ϵ�����������
    integer(kind=4) number_row_K_global
    
    !�����ڽڵ��ϵķֲ��غɣ����귽�򣩵ĸ���
    integer(kind=4) number_pressure
    
    !�����ڽڵ��ϵķֲ��غɵ���Ϣ����
    !information_pressure(1,i)����i���ֲ��غ����õĽڵ��
    !information_pressure(2,i) = 1������X����ķֲ���
    !information_pressure(2,i) = 0������Y����ķֲ���
    integer(kind=4) information_pressure(2, number_pressure)
    
    !����ѭ������
    integer(kind=4) i, j
    
    !���÷ֲ�����ֵ�������귽��Ϊ����
    real(kind=8) value_pressure(number_pressure)
    
    write(*,"(/,1X,'��distributed_load_input�������ӳ�����ʱ��֧�ֵ��á�')")
        
end