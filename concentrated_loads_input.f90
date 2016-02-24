!****************************************************************************************************
!����ԭ������Ԫ���ӳ��������������뼯���غ����ݲ��������ļ��������ʽ���������£�
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
subroutine concentrated_loads_input(number_force)

!ʹ��afem_parameterģ�飨���ж����˲�����
use afem_parameter

!�������б�����Ӧ����ʦ�����и����ı���
!number_force       =>  NF
!information_force  =>  MF
!value_force        =>  ZF

!Ĭ���������Ҫ��������
implicit none

    !�������
    
    !�����ڽڵ��ϵļ����غɣ����귽�򣩵ĸ���
    integer(kind=4) number_force
    
    !����ѭ������
    integer(kind=4) i, j, k
    
    !�����ڽڵ��ϵļ����غɵ���Ϣ����
    !information_force(1,i)����i�������غ����õĽڵ��
    !information_force(2,i) = 1������X����ļ�����
    !information_force(2,i) = 2������Y����ļ�����
    !information_force(2,i) = 3������Y����ļ�����
    integer(kind=4) information_force(2, number_force)
    
    !���ü�������ֵ�������귽��Ϊ����
    real(kind=8) value_force(number_force)
    
    !����ʼֵ
    information_force(1,:) = 0
    information_force(2,:) = 0
    
    write(*,"(1X, '��Ҫ����Ľڵ��� = ', I5)") amount_node_clear
    
    write(*,"(/, 1X, '************ ��ʼ���뼯���غ� ************')")
    !��ȡ���м����غɸ�������Ϣ��ֵ���ļ�
    do i = 1, number_force
        write(*,"('������information_force(1,i)����i�������غ����õĽڵ��')")
        read(*,*) information_force(1,i)
        !�ж�information_force(1,i)�е���ֵ�Ƿ����
        do while((information_force(1,i).LE.0).OR.(information_force(1,i).GE.(DOF + 3*amount_node_clear)/3+1))
            
            write(*,"(1X, '����������ԭ�������������������롣')")
            information_force(1,i) = 0
            read(*,*) information_force(1,i)
            
        enddo
        
        do while(.true.)
        
        !�ж�ԭ���Ƿ�Ϊ����ԭ��
            do k = 1, amount_node_clear
                
                if(information_force(1,i) .EQ. number_node_clear(k)) then
                    
                    write(*,"(1X, '����������ԭ��Ϊ����ԭ�ӣ����������롣')")
                    information_force(1,i) = 0
                    read(*,*) information_force(1,i)
                    
                endif
                
            enddo
            
            !�˳�����ѭ��
            exit
            
        enddo
        
        write(*,"('������information_force(2,i)��', /, ' = 1����X�������õļ�����', /, ' = 2����Y�������õļ�����', /, ' = 3����Z�������õļ�����')")
        read(*,*) information_force(2,i)
        !�ж�information_force(2,i)�е���ֵ�Ƿ����
        do while((information_force(2,i).NE.1).AND.(information_force(2,i).NE.2).AND.(information_force(2,i).NE.3))
            
            write(*,"(1X, '���������������������������롣')")
            !write(*,"(' = 1����X�������õļ�����', /, ' = 2����Y�������õļ�����', /, ' = 3����Z�������õļ�����')")
            information_force(2,i) = 0
            read(*,*) information_force(2,i)
            
        enddo
        
        write(*,"('������value_force(i)����i���������Ĵ�С')")
        read(*,*) value_force(i)
        
        !read(unit=5,fmt=*)����Ӽ�������
        !read(5,*)(information_force(j,i), j = 1,2), value_force(i)
    enddo
    
    write(*,"(1X, '************ �����غ�������� ************', /)")
    
    write(*,"(/, 1X, '********** �����غɿ�ʼд���ļ� **********')")
    open(1,file='concentrated_loads.txt')
    
    !����ַ���'34HCONCENTRATED LOADS information_force(3,NF) value_force(NF)'
    write(1,"(1X, 'concentrated loads information_force(3,number_force) value_force(number_force)')")
    
    !������м�������Ӧ�ģ�
    !information_force(1,i)��information_force(2,i)��value_force(i)
    !information_force(1,i)����i�������غ����õĽڵ��
    !information_force(2,i) = 1������X����ļ�����
    !information_force(2,i) = 2������Y����ļ�����
    !information_force(2,i) = 3������Y����ļ�����
    do i = 1, number_force
        write(1,"(/,2I10,F20.6)") (information_force(j,i),j=1,2), value_force(i)
    enddo
    
    close(1)
    write(*,"(/, 1X, '- \ | / - \ | / - \ | / - \ | / - \ | /')")
    write(*,"(/, 1X, '********** �����غ�д���ļ����� **********')")
    
    !�����ӳ��򽫼����غ�������װ����ȫ��������
    call concentrated_loads_assembling(number_force, information_force, value_force)
    
end