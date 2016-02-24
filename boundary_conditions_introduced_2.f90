!****************************************************************************************************
!����ԭ������Ԫ���ӳ��������������ñ߽�����������λ��ֵ�������ܸ������ԣ������ʽ���������£�
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
subroutine boundary_conditions_introduced_2(number_given_displacement)

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
    integer(kind=4) information_given_displacement(2, number_given_displacement)
    
    !
    integer(kind=4) S
    !����ѭ������
    integer(kind=4) i, j
    !����m�洢��i���غ����õķ��򣬱���n�洢��i���غ����õĽڵ��
    integer(kind=4) m, n
    !����mn�洢�غɶ�Ӧ�ڵ㡢��Ӧ���������
    !����mn��Ӧcoordinate_nodes_deformed��coordinate_nodes_initial������n�Žڵ㡢m���������
    integer(kind=4) mn

    
    !����λ�Ʊ߽�������ֵ
    !
    real(kind=8) value_given_displacement(number_given_displacement)
    !
    real(kind=8) Z
    
    !����ά�ȿ�����洢�ĸն���
    real(kind=8) K_global_bandwidth(DOF, DOF)
    
    !��ʼ�ڵ�����
    real(kind=8) coordinate_nodes_initial(DOF)
    !���κ�ڵ�����
    real(kind=8) coordinate_nodes_deformed(DOF)
    
    !
    real(kind=8) FORCE(DOF)
    !������force_of_node�ڱ��ӳ����еĸ���
    real(kind=8) FACTOR
    !
    real(kind=8) FF(DOF)
    !
    real(kind=8) ERRF
    
    !������ʼ������force_of_node��ֵ��FORCE
    FORCE = force_of_node
    !������ʼ��
    FF = 1.0
    
    !��һ��ѭ����
    !1==============================================================================
    do i = 1, number_given_displacement
        !����i���غ����õĽڵ�Ÿ�ֵ������n
        n = information_given_displacement(1,i)
        !����i���غ����õķ���ֵ������m
        m = information_given_displacement(2,i)
        !����mn��Ӧcoordinate_nodes_deformed��coordinate_nodes_initial������n�Žڵ㡢m���������
        mn = 3*(n-1) + m
        
        Z = (value_given_displacement(i) * FACTOR - (coordinate_nodes_deformed(mn) - coordinate_nodes_initial(mn)))
        
        S = 0
        FORCE(mn) = 0.0
        FF(mn) = 0.0
        
        !�ڶ���ѭ����
        !2==========================================================================
        do j = 2, DOF
            !dabs��˫���ȱ���ȡ����ֵ�ĺ���
            S = S + dabs(K_global_bandwidth(mn, j))
            if(mn + 1 - j.GT.0) then
                S = S + dabs(K_global_bandwidth(mn + 1 - j, j))
            endif
        enddo
        !2==========================================================================
        !K_global_bandwidth(mn, 1)���һ�����������
        K_global_bandwidth(mn, 1) = S*1.E+15+1
        force_of_node(mn) = K_global_bandwidth(mn, 1) * Z
    enddo
    !1==============================================================================
    ERRF = 0.0
    do i = i, DOF
        if(ERRF.LT.dabs(FORCE(i))) then
            ERRF = dabs(FORCE(i))
        endif
    enddo
    write(*,"(1X,'����ݶ�����MAX RESIDUAL FORCE = ')")
end