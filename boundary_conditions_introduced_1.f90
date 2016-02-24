!****************************************************************************************************
!����ԭ������Ԫ���ӳ��������������ñ߽���������λ��ֵ�������ܸ������ԣ������ʽ���������£�
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
subroutine boundary_conditions_introduced_1(number_given_displacement)

!�������б�����Ӧ����ʦ�����и����ı���
!DOF                            =>  NT
!DOF                            =>  ND
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
    
    !�o��λ�������ĸ���
    integer(kind=4) number_given_displacement
    !����λ�Ʊ߽���������Ϣ
    !information_given_displacement(1,i)����i���̶�λ�ƵĽڵ��
    !information_given_displacement(2,i) = 1��X�������λ��
    !information_given_displacement(2,i) = 2��Y�������λ��
    !information_given_displacement(2,i) = 3��Z�������λ��
    integer(kind=4) information_given_displacement(2, number_given_displacement)
    
    !
    integer(kind=4) S
    !����ѭ������
    integer(kind=4) i, j
    !����m�洢��i���غ����õķ��򣬱���n�洢��i���غ����õĽڵ��
    integer(kind=4) m, n, a, b, c, d
    !����mn�洢�غɶ�Ӧ�ڵ㡢��Ӧ���������
    !����mn��Ӧcoordinate_nodes_deformed��coordinate_nodes_initial������n�Žڵ㡢m���������
    integer(kind=4) mn

    
    !����λ�Ʊ߽�������ֵ
    !���ӳ���boundary_conditions_introduced_1�ж�����λ�����öԽ�Ԫ�ظ�һ�������ն���������
    real(kind=8) value_given_displacement(number_given_displacement)

    
    !����ʼֵ
    information_given_displacement = 0
    !�̶�λ�ƣ�1ԭ��X����
    information_given_displacement(1,1) = 1
    information_given_displacement(2,1) = 1
    !�̶�λ�ƣ�9ԭ��Y����
    !information_given_displacement(1,2) = 9
    !information_given_displacement(2,2) = 2
    !�̶�λ�ƣ�17ԭ��Z����
    information_given_displacement(1,3) = 12
    information_given_displacement(2,3) = 3
    !�̶�λ�ƣ�1ԭ��Y����
    information_given_displacement(1,2) = 15
    information_given_displacement(2,2) = 2
    !�̶�λ�ƣ�1ԭ��Y����
    !information_given_displacement(1,5) = 1
    !information_given_displacement(2,5) = 3
    !�̶�λ�ƣ�1ԭ��Y����
    !information_given_displacement(1,2) = 9
    !information_given_displacement(2,2) = 3
    
    !���ӳ���boundary_conditions_introduced_1�ж�����λ�����öԽ�Ԫ�ظ�һ�������ն���������
    value_given_displacement(number_given_displacement) = 0.0
    
    !��һ��ѭ����
    !1==============================================================================
    do i = 1, number_given_displacement
        !����i���غ����õĽڵ�Ÿ�ֵ������n
        n = information_given_displacement(1,i)
        !����i���غ����õķ���ֵ������m
        m = information_given_displacement(2,i)
        
        !c = 0
        d = 0
        
        !����ڵ��ų���Ҫ����Ľڵ�žͼ�ȥ
        do b = 1, amount_node_clear
            if(n .GT. number_node_clear(b)) then
                d = d + 1
            !elseif(j .EQ. number_node_clear(b)) then
            !    j = 1
            endif
        enddo
        n = n - d

        
        !����mn��Ӧcoordinate_nodes_deformed��coordinate_nodes_initial������n�Žڵ㡢m���������
        mn = 3*(n-1) + m
        
        !��ȫ�ָն���ĵ�mn�к͵�mn������
        K_global(mn,:) = 0.0
        K_global(:,mn) = 0.0
        !��K_global(mn,mn)����Ϊ1
        K_global(mn,mn) = 1.0
        
        !��force_of_node_cleared(mn)����
        force_of_node_cleared(mn) = 0.0
        
    enddo
        
end