!****************************************************************************************************
!����ԭ������Ԫ���ӳ���������������ֲ��غ����ݲ��������ļ��������ʽ���������£�
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
subroutine distributed_loads_input(number_pressure)

!�������б�����Ӧ����ʦ�����и����ı���
!number_pressure       =>  NP
!information_pressure  =>  MP
!value_pressure        =>  ZP

!Ĭ���������Ҫ��������
implicit none

    !�������
    
    !�����ڽڵ��ϵķֲ��غɣ����귽�򣩵ĸ���
    integer(kind=4) number_pressure
    
    !�����ڽڵ��ϵķֲ��غɵ���Ϣ����
    !information_pressure(1-2,number_pressure)������ʱ��˳��洢�����غ�ʩ�ӵĽڵ���
    !information_pressure(3,i) = 0������X����ľ�����
    !information_pressure(3,i) = 1������Y����ľ�����
    !information_pressure(3,i) = 2��������������������
    !information_pressure(4,i)����i���غ���
    integer(kind=4) information_pressure(4, number_pressure)
    
    !����ѭ������
    integer(kind=4) i, j
    
    !���÷ֲ�����ֵ�������귽��Ϊ����
    real(kind=8) value_pressure(number_pressure)
    
    !��ȡ���зֲ��غɸ�������Ϣ��ֵ���ļ�
    do i = 1, number_pressure
        !read(unit=5,fmt=*)����Ӽ�������
        read(5,*)(information_pressure(j,i), j = 1,2), value_pressure(i)
    enddo
    
    !����ַ���'34HCONCENTRATED LOADS information_pressure(3,NF) value_pressure(NF)'
    !������������������������������������������������������������������������������������������������������������������������
    !Ӧ�ô򿪶�����豸��Ϊ6���ļ���open(unit=6, file='results.txt')
    write(6,"(/,10X,'29HUNIFORM LOADS MP(4,NP) ZP(NP)')")
    !������������������������������������������������������������������������������������������������������������������������
    
    !������зֲ�����Ӧ�ģ�
    !information_pressure(1-2,number_pressure)������ʱ��˳��洢�����غ�ʩ�ӵĽڵ���
    !information_pressure(3,i) = 0������X����ľ�����
    !information_pressure(3,i) = 1������Y����ľ�����
    !information_pressure(3,i) = 2��������������������
    !information_pressure(4,i)����i���غ���
    do i = 1, number_pressure
        !������������������������������������������������������������������������������������������������������������������������
        !Ӧ�ô򿪶�����豸��Ϊ6���ļ���open(unit=6, file='results.txt')
        write(6,"(/,2I10,F20.6)") (information_pressure(j,i),j=1,2), value_pressure(i)
        !������������������������������������������������������������������������������������������������������������������������
    enddo
    
end