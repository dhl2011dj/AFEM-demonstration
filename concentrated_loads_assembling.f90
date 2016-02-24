!****************************************************************************************************
!����ԭ������Ԫ���ӳ�������������װ�����غ����ݲ������������������ʽ���������£�
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
subroutine concentrated_loads_assembling(number_force, information_force, value_force)

!ʹ��afem_parameterģ�飨���ж����˲�����
use afem_parameter

!�������б�����Ӧ����ʦ�����и����ı���
!number_force       =>  NF
!information_force  =>  MF
!value_force        =>  ZF
!number_row_K_global=>  NT
!force_of_node      =>  F

!Ĭ���������Ҫ��������
implicit none

    !�������
    
    !�����ڽڵ��ϵļ����غɣ����귽�򣩵ĸ���
    integer(kind=4) number_force
    
    !�����ڽڵ��ϵļ����غɵ���Ϣ����
    !information_force(1,i)����i�������غ����õĽڵ��
    !information_force(2,i) = 1������X����ļ�����
    !information_force(2,i) = 2������Y����ļ�����
    !information_force(2,i) = 3������Y����ļ�����
    integer(kind=4) information_force(2, number_force)
    
    !����ѭ������
    integer(kind=4) i, j
    
    !���ü�������ֵ�������귽��Ϊ����
    real(kind=8) value_force(number_force)
    
    write(*,"(/, 1X, '********** �����غɿ�ʼ����ȫ�������� **********')")
    
    do i = 1, number_force
        !j�����i�������غ����õĽڵ��
        j = information_force(1,i)
        !force_of_node������Ϊnumber_row_K_global��������
        !ÿ���ڵ����3�����������λ�����
        !���information_force(2,i) = 
        !������������������������������������������������������������������������������������������������������������������������
        force_of_node(3*(j - 1) + information_force(2,i)) = force_of_node(3*(j - 1) + information_force(2,i)) + value_force(i)
        !���Ϲ涨information_force(2,i) = 1����X�������� = 0����Y�����������о������⣬���������뱾���򲻷���
        !����Ϊ���±�����
        !1.force_of_node(3*(j - 1) + information_force(2,i)) = force_of_node(3*(j - 1) + information_force(2,i)) + value_force(i)
        !����֣�Ϊʲôû��Z����ļ�����������
        !������ʽ��information_force(2,i) = 1����X�������� = 2����Y�������� = 3����Z��������
        
        !����
        !2.force_of_node(3*j - information_force(2,i)) = force_of_node(3*j - information_force(2,i)) + value_force(i)
        !����֣�Ϊʲôû��Z����ļ�����������
        !������ʽ��information_force(2,i) = 2����X�������� = 1����Y�������� = 0����Z��������
        !��ʽ2��������Щ����
        !������������������������������������������������������������������������������������������������������������������������
    enddo
    
    write(*,"(/, 1X, '- \ | / - \ | / - \ | / - \ | / - \ | /')")
    write(*,"(/, 1X, '********** �����غɷ���ȫ����������� **********', ///)")
    
    write(*,"(/, 1X, '����������ѡ�����롮exit�������������غɵ�����')")
    write(*,"(1X, '���߼�������߽����������ͣ�[a]�������غɱ߽�������[b]���ֲ��غɱ߽�����', /)")
    
end