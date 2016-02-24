!****************************************************************************************************
!����ԭ������Ԫ���ӳ����������ڵ��ü��С��ֲ��غ��ӳ�����б߽��������ݵ�����ʹ洢�������ʽ���������£�
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
subroutine loads_input()
!ʹ��afem_parameterģ�飨���ж����˲�����
use afem_parameter

!Ĭ���������Ҫ��������
implicit none

    !�������
    
    !����ϵ�����������
    integer(kind=4) number_row_K_global
    
    !�ñ������ڴ洢�غɱ߽������������Լ��жϱ߽������Ƿ��������
    character(len=4) judge_end_boundary_condition
    
    !�����ڽڵ��ϵļ����غɣ����귽�򣩵ĸ���
    integer(kind=4) number_force
    
    !�����ڽڵ��ϵķֲ��غɣ����귽�򣩵ĸ���
    integer(kind=4) number_pressure
    
    !Ϊ���ɵ�ȫ�ֽڵ������������ڴ�
    allocate(force_of_node(DOF + 3*amount_node_clear))
    !����ʼֵ
    force_of_node = 0.0
    
    !ϵ�������������ȫ�����ɶ���
    number_row_K_global = DOF
    
    !write(*,"(/,'2.**************************************************************',/,1X,'��ʼ����߽�������',/)")
    !
    write(*,"(1X,'������߽����������ͣ�',/,1X,'[a]�������غɱ߽�������[b]���ֲ��غɱ߽�����')")
    write(*,"(1X,'�����Ҫ�߽�����������������룺exit',/)")
    !�߽����������룬ֻ�е������ַ���exit����ʱ��ѭ���˳�������һֱҪ�������غɱ߽�����
    do while(.true.)
        
        read(*,*) judge_end_boundary_condition
        if(judge_end_boundary_condition.NE.'exit') then
            !���judge_end_boundary_condition = 1˵��Ҫ���뼯���غ�
            if(judge_end_boundary_condition.EQ.'a') then
                write(*,"(/,1X,'���������ĸ�����number_force')")
                read(*,*) number_force
                
                !�����ӳ�����м����غ����ݵ�������ļ�����
                call concentrated_loads_input(number_force)
                
            !���judge_end_boundary_condition = 2˵��Ҫ��������غ�
            elseif(judge_end_boundary_condition.EQ.'b') then
                write(*,"(1X,'�ݲ�֧�־����غɵ�����',/)")
                !�����ӳ�����о����غ����ݵ�������ļ�����
                !call distributed_loads_input(number_pressure)
                !�����ӳ����������غ����ݵĵ�Ч�ڵ���������Ч�ڵ���������������
                !call distributed_loads_assembling(number_pressure, number_row_K_global)
            else
                write(*,"(1X,'����������������������',/)")
            endif
            
        elseif(judge_end_boundary_condition.EQ.'exit') then
            exit
        endif
    enddo
    !
    !call 
    
    !write(*,"(/,1X,'�߽��������������',/,'2.**************************************************************',/)")
    
    
end