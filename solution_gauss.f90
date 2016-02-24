!****************************************************************************************************
!����ԭ������Ԫ���ӳ�����������������ѡԪGauss��ȥ��(����Ԫ)������Է����飺
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
subroutine solution_gauss()

!�������б�����Ӧ����ʦ�����и����ı���
!DOF            =>  NT
!DOF         =>  ND
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
!ʹ��inverse_matrixģ�飬���а����˸�˹��ȥ����ϵ������������������Ԫ���̵��ӳ���
use inverse_matrix

!Ĭ���������Ҫ��������
implicit none

    !�������
    
    !ȫ�ָն���
    !real(kind=8) K_global(DOF, DOF)
    
    !����ڵ�λ������
    real(kind=8) displacement_of_node(DOF)
    
    !ȫ��������
    real(kind=8) force_of_node_2(DOF)
    
    !����ѭ������
    integer(kind=4) i, j, m, n
    
    !����CPUռ�ü�ʱ����
    real(kind=8) time_begin, time_end
    
    open(1, file = 'time_solution_gauss.txt')
    
    !CPU��ʱ���򣨼�ʱ��ʼ��
    call CPU_TIME(time_begin)
    
    !Ϊforce_of_node_cleared�����ڴ�ռ�
    allocate(X(DOF))
    
    !���ø�˹��ȥ���������Ԫ����matrix_gauss(A,B,X,n,m)
    !�ӳ���matrix_gauss(A,B,X,n,m)��
    force_of_node_2 = force_of_node
    
    call matrix_gauss(K_global, force_of_node_cleared, displacement_of_node, DOF, 1)
    
    !��ֵ
    X = displacement_of_node
    
    !CPU��ʱ���򣨼�ʱ������
    call CPU_TIME(time_end)
    
    !���CPUռ��ʱ��
    write(1,"(1X, 'solution_gauss�ӳ����ܹ�ռ��CPUʱ��Ϊ��', F, 's')") time_end - time_begin
    
    close(1)
    
end