!****************************************************************************************************
!����ԭ������Ԫ���ӳ�������������װȫ�ָն���
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
subroutine K_global_assembling()

!�������б�����Ӧ����ʦ�����и����ı���
!number_element             =>  IO
!amount_all_elements        =>  NE      
!amount_all_nodes           =>  NG
!node_of_element            =>  IJM
!                           =>  TE
!coordinate_nodes_initial   =>  XY
!coordinate_nodes_deformed  =>  CXY
!element_force              =>  EF
!current_element_force      =>  CFE
!coordinate_node_of_element =>  EX
!node_number_of_each_element=>  NELE
!K_element                  =>  EK


!ʹ��afem_parameterģ�飨���ж����˲�����
!��Ԫ�ն��������K_element_row = 6����Ԫ�ն��������K_element_column = 6
use afem_parameter

!Ĭ���������Ҫ��������
implicit none

    !�������
    
    !��Ԫ���
    integer(kind=4) number_element
    
    !TE
    !integer(kind=4) TE(number_element)
    !node_number_of_each_element(i)�洢��i����Ԫ�е�amount_node_of_element���ڵ��
    integer(kind=4) node_number_of_each_element(amount_node_of_element)
    
    !��ʵ��ȫ�����ɶ�
    integer(kind=4) DOF_global
    
    !��������е�ѭ������
    integer(kind=4) i, j, k, m, n, a, b, c, d
    
    !�ڵ���������
    !coordinate_nodes_initial(1,I)�����ʼʱ��I���ڵ��X����
    !coordinate_nodes_initial(2,I)�����ʼʱ��I���ڵ��Y����
    !coordinate_nodes_initial(3,I)�����ʼʱ��I���ڵ��Z����
    real(kind=8) coordinate_nodes_initial(3, DOF/3)
    !���κ�ڵ���������
    !coordinate_nodes_deformed(1,I)������κ��I���ڵ��X����
    !coordinate_nodes_deformed(2,I)������κ��I���ڵ��Y����
    !coordinate_nodes_deformed(3,I)������κ��I���ڵ��Z����
    real(kind=8) coordinate_nodes_deformed(3, DOF/3)
    !���ڵĵ�Ԫ������
    real(kind=8) current_element_force(K_element_row)
    !��Ԫ�նȾ���
    !K_element_row = 6,K_element_column = 6
    real(kind=8) K_element(K_element_row,K_element_column)
    
    !coordinate_node_of_element(i, j)�洢ÿ����Ԫ��i�Žڵ������ֵj = 1, 3�ֱ����i�Žڵ������X��Y��Zֵ
    !amount_node_of_element = 2
    real(kind=8) coordinate_node_of_element(amount_node_of_element,3)
    !��Ԫ��������
    real(kind=8) element_force(K_element_row)
    
    !Ϊȫ�ָն�������ڴ�ռ�
    !DOF = DOF - 3*amount_node_clear; allocate(K_global(DOF, DOF))
    !DOF_global = DOF - 3*amount_node_clear; allocate(K_global(DOF_global, DOF_global))
    
    !##############################################################�����п�ʼ��
    !���岢�м������
    integer(kind=4) nthreads, tid, omp_get_num_threads, omp_get_thread_num, chunksize, chunk
    parameter (chunksize = 10)
    !����CPUռ�ü�ʱ����
    real(kind=8) time_begin, time_end
    
    open(1, file = 'time_K_global_assembling.txt')
    
    !CPU��ʱ���򣨼�ʱ��ʼ��
    call CPU_TIME(time_begin)
    
    chunk = chunksize
    
    !openMP��ʼ
    !$omp parallel shared(nthreads) private(i,j,number_element)
    !##############################################################�����п�ʼ��
    
    !����ʼֵ
    K_global = 0.0
    
    tid = omp_get_thread_num()
    
    if (tid .eq. 0) then
    nthreads = omp_get_num_threads()
    print *, 'number of threads =', nthreads
    end if
    print *, 'thread',tid,' starting...'
    
    !$omp do schedule(dynamic,chunk)
    
    do number_element = 1, amount_all_elements
        write(*,"(1X, 'amount_all_elements = ', I5)") amount_all_elements
        !�����ӳ������ɵ�Ԫ�ն���
        !call K_element_assembling(number_element, amount_all_elements, DOF/3, node_of_element, TE, coordinate_nodes_initial, coordinate_nodes_deformed, current_element_force, K_element)
        call K_element_assembling(number_element, K_element)
        !����Ԫ�ն�����ϳ�Ϊȫ�ָն���
        !����number_element����Ԫ�нڵ�Ľڵ�Ÿ�ֵ��node_number_of_each_element����
        node_number_of_each_element(:) = main_node_of_element(:, number_element)
        
        !����Ԫ�е������ڵ�ŷֱ𸳸�i, j
        i = node_number_of_each_element(1)
        j = node_number_of_each_element(2)
        
        !write(*,"(1X, 'i = ', I5)") i
        !write(*,"(1X, 'j = ', I5)") j
        
        !ÿ�ν����µ�Ԫ�����¸�c��d��ֵ
        c = 0
        d = 0
        
        !����ڵ��ų���Ҫ����Ľڵ�žͼ�ȥ
        do a = 1, amount_node_clear
            if(i .GT. number_node_clear(a)) then
                c = c + 1
            !elseif(i .EQ. number_node_clear(a)) then
            !    i = 1
            endif
        enddo
        i = i - c
        
        !����ڵ��ų���Ҫ����Ľڵ�žͼ�ȥ
        do b = 1, amount_node_clear
            if(j .GT. number_node_clear(b)) then
                d = d + 1
            !elseif(j .EQ. number_node_clear(b)) then
            !    j = 1
            endif
        enddo
        j = j - d
        
        !write(*,"(1X, 'i* = ', I5)") i
        !write(*,"(1X, 'j* = ', I5)") j
        
        !m, n�ֱ���i�ڵ��j�Žڵ��X/Y/Z�����ѭ������
        do m = 1, 3
            do n = 1, 3
                K_global(3*(i-1)+m, 3*(i-1)+n) = K_global(3*(i-1)+m, 3*(i-1)+n) + K_element(m, n)
            enddo
        enddo
        
        !m, n�ֱ���i�ڵ��j�Žڵ��X/Y/Z�����ѭ������
        do m = 1, 3
            do n = 1, 3
                K_global(3*(i-1)+m, 3*(j-1)+n) = K_global(3*(i-1)+m, 3*(j-1)+n) + K_element(m, 3+n)
            enddo
        enddo
        
        !m, n�ֱ���i�ڵ��j�Žڵ��X/Y/Z�����ѭ������
        do m = 1, 3
            do n = 1, 3
                K_global(3*(j-1)+m, 3*(i-1)+n) = K_global(3*(j-1)+m, 3*(i-1)+n) + K_element(3+m, n)
            enddo
        enddo
        
        !m, n�ֱ���i�ڵ��j�Žڵ��X/Y/Z�����ѭ������
        do m = 1, 3
            do n = 1, 3
                K_global(3*(j-1)+m, 3*(j-1)+n) = K_global(3*(j-1)+m, 3*(j-1)+n) + K_element(3+m, 3+n)
            enddo
        enddo
    enddo
    
    !$omp enddo nowait
    
    !���ȫ�ָն����ά��
    write(*,"('ȫ�ָնȾ����ά��Ϊ��', I10, '          *', I10)") DOF, DOF
    
    print *, 'thread',tid,' done.'
    
    !##############################################################�����н�����
    !openMP����
    !$omp end parallel
    
    !CPU��ʱ���򣨼�ʱ������
    call CPU_TIME(time_end)
    
    !���CPUռ��ʱ��
    write(1,"(1X, 'K_global_assembling�ӳ����ܹ�ռ��CPUʱ��Ϊ��', F, 's')") time_end - time_begin
    
    close(1)
    !##############################################################�����н�����
end