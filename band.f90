!****************************************************************************************************
!����ԭ������Ԫ���ӳ�����������������ѡ��ԪGauss��ȥ��(����Ԫ)������Է����飺
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
subroutine solution_band(number_row_K_global, number_column_K_global, K_global, force_of_node, )

!�������б�����Ӧ����ʦ�����и����ı���
!number_row_K_global            =>  NT
!number_column_K_global         =>  ND
!number_given_dislpacement      =>  NB
!information_given_dislpacement =>  MB
!value_given_dislpacement       =>  ZB
!K_global_bandwidth             =>  SK
!force_of_node                  =>  F
!coordinate_nodes_initial       =>  XY
!coordinate_nodes_deformed      =>  CXY
!                               =>  FORCE
!                               =>  FACTOR
!                               =>  FF
!                               =>  ERRF

!m                              =>  M
!n                              =>  N
!mn                             =>  NN

!ʹ��afem_parameterģ�飨���ж����˲�����
use afem_parameter

!Ĭ���������Ҫ��������
implicit none

    !�������
    
    !����ϵ�����������
    integer(kind=4) number_row_K_global
    !����ϵ�����������
    integer(kind=4) number_column_K_global
    
    !ȫ�ָն���
    real(kind=8) K_global(number_row_K_global, number_column_K_global)
    !ȫ��������
    real(kind=8) force_of_node(number_row_K_global)
    
    !����ѭ������
    integer(kind=4) i, j, m, n
        
    

end