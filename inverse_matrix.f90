!****************************************************************************************************
!����ԭ������Ԫ��ģ�飬������ʹ�ø�˹����Ԫ��ȥ�����㣺
!1. inv_matrix(A, inv_A, n)���������A�������
!2. matrix_gauss(A, B, X, n, m)����������Ԫ����
!****************************************************************************************************
module inverse_matrix
contains
    !�������A�������
    subroutine inv_matrix(A, inv_A, n)
    !****************************************************************************************************
    !���������input����
    !1. A����Ҫ����ķ���
    !2. n�������ά��
    !���������output����
    !1. inv_A������A�������
    !�½�������new����
    !
    !�����ӳ��򣨺�������
    !
    !�������ӳ��򣨺�������
    !
    !��д�ļ�����
    !
    !****************************************************************************************************
    implicit none
    
    !���巽���ά��n
    integer(kind=4) n
    
    !����ѭ������
    integer(kind=2) i,j
    
    !����A
    real(kind=8) A(n,n)
    !�����inv_A
    real(kind=8) inv_A(n,n)
    !��λ����
    real(kind=8) E(n,n)
    
    !����ʼֵ
    E = 0.0
    do i = 1, n
        !����λ����ֵ
        E(i,i) = 1.0
    enddo
    
    call matrix_gauss(A, E, inv_A, n, n)
    
    !���ϵ������A�������inv_A
    !write(*,"(/,1X, 'inv_A = ')")
    !write(*,"(1X, 4F8.4)")((inv_A(i,j),j = 1,n),i = 1,n)
    
    end subroutine inv_matrix
    
    !����Gauss��ȥ��������󷽳̣���m�����������ֿ����㣩
    subroutine matrix_gauss(A, B, X, n, m)
    !****************************************************************************************************
    !���������input����
    !1. A����Ҫ����ķ���
    !2. n�������ά��
    !3. B��n��m�г�������
    !4. m����������B�����������B���������Ļ���m = 1
    !5. X��n��m�еĽ�������B����������m = 1�Ļ���XΪ������
    !���������output����
    !1. X��n��m�еĽ�������B����������m = 1�Ļ���XΪ������
    !�½�������new����
    !
    !�����ӳ��򣨺�������
    !
    !�������ӳ��򣨺�������
    !
    !��д�ļ�����
    !
    !****************************************************************************************************
    implicit none
    
    !��������ά��n
    integer(kind=4) n, m
    
    !����ѭ������
    integer(kind=2) i, j
    
    !����A
    real(kind=8) A(n,n)
    !
    real(kind=8) B(n,m)
    !
    real(kind=8) X(n,m)
    !
    real(kind=8) X_temp(n), b_temp(n)
    
    !##############################################################�����п�ʼ��
    !���岢�м������
    integer(kind=4) nthreads, tid, omp_get_num_threads, omp_get_thread_num, chunksize, chunk
    parameter (chunksize = 100)
    !����CPUռ�ü�ʱ����
    real(kind=8) time_begin, time_end
    
    open(1, file = 'time_matrix_gauss.txt')
    
    !CPU��ʱ���򣨼�ʱ��ʼ��
    call CPU_TIME(time_begin)
    
    chunk = chunksize
    
    !openMP��ʼ
    !$omp parallel shared(A, B, X, X_temp, b_temp) private(i,j)
    !##############################################################�����п�ʼ��
    
    do i = 1, m
        
        !��B���з���Btemp���Խ������
        b_temp(:) = B(: , i)
        
        call elgauss(A, b_temp, X_temp, n)
        !�����õ���Xtemp���з���X��
        X(:,i) = X_temp
        
    enddo
    
    !##############################################################�����н�����
    !openMP����
    !$omp end parallel
    
    !CPU��ʱ���򣨼�ʱ������
    call CPU_TIME(time_end)
    
    !���CPUռ��ʱ��
    write(1,"(1X, 'matrix_gauss�ӳ����ܹ�ռ��CPUʱ��Ϊ��', F, 's')") time_end - time_begin
    
    close(1)
    !##############################################################�����н�����
    !��������X
    !write(*,"(/,1X, '�����X = ')")
    !write(*,"(1X, 4F8.4)")((X(i,j),j = 1,m),i = 1,n)
    
    end subroutine matrix_gauss
    
    !Gauss����Ԫ��ȥ������ϵ��������Ԫ��Ϊ��������
    subroutine elgauss(A, b, x, n)
    !****************************************************************************************************
    !���������input����
    !1. A����Ҫ����ķ���
    !2. n�������ά��
    !���������output����
    !1. inv_A������A�������
    !�½�������new����
    !
    !�����ӳ��򣨺�������
    !
    !�������ӳ��򣨺�������
    !
    !��д�ļ�����
    !
    !****************************************************************************************************
    !
    implicit none
    
    !��������ά��n
    integer(kind=4) n
    
    !��Ԫ�ر��
    integer(kind=4) id_max
    
    !����ѭ������
    integer(kind=2) i, k
    
    !����Ԫ��ÿ������������
    real(kind=8) element_max
    !����A
    real(kind=8) A(n,n)
    !���еĳ�������
    real(kind=8) b(n)
    !
    real(kind=8) x(n)
    
    real(kind=8) A_up(n,n), b_up(n)
    !�������[Ab]
    real(kind=8) Ab(n,n+1)
    !
    real(kind=8) V_temp1(n+1), V_temp2(n+1)
    !����Ԫ�����ж�����Ԫ�ı�������
    real(kind=8) factor
    
    !##############################################################�����п�ʼ��
    !���岢�м������
    integer(kind=4) nthreads, tid, omp_get_num_threads, omp_get_thread_num, chunksize, chunk
    parameter (chunksize = 100)
    !����CPUռ�ü�ʱ����
    real(kind=8) time_begin, time_end
    
    open(1, file = 'time_elgauss.txt')
    
    !CPU��ʱ���򣨼�ʱ��ʼ��
    call CPU_TIME(time_begin)
    
    chunk = chunksize
    
    !openMP��ʼ
    !$omp parallel shared(element_max, A, b, x, A_up, b_up, Ab, V_temp1, V_temp2) private(i,j)
    !##############################################################�����п�ʼ��
    
    !��A�����b�������ɵ����������ȥ
    Ab(1:n,1:n) = A
    Ab(:,n+1) = b
    
    !��������Ԫ��ѡȡ������k�к�i��Ԫ�ص�λ��
    do k = 1, n-1
        !ѡȡ����Ԫ������Ӧ���кţ�����element_max��id_max
        element_max = dabs(Ab(k,k))
        id_max = k
        do i = k+1, n
            if(dabs(Ab(i,k)).GT.element_max) then
                element_max = Ab(i,k)
                id_max = i
            endif
        enddo
        !����k��Ԫ�غ�����Ԫ�����е�Ԫ��
        V_temp1 = Ab(k,:)
        V_temp2 = Ab(id_max,:)
        Ab(k,:) = V_temp2
        Ab(id_max,:) = V_temp1
        
        
        do i = k+1, n
            !��һ��
            factor = Ab(i,k) / Ab(k,k)
            !��Ԫ
            Ab(i,:) = Ab(i,:) - factor * Ab(k,:)
        enddo
    enddo
    
    A_up(:,:) = Ab(1:n,1:n)
    b_up(:) = Ab(:,n+1)
    
    call up_tri(A_up, b_up, x, n)
    
    !##############################################################�����н�����
    !openMP����
    !$omp end parallel
    
    !CPU��ʱ���򣨼�ʱ������
    call CPU_TIME(time_end)
    
    !���CPUռ��ʱ��
    write(1,"(1X, 'elgauss�ӳ����ܹ�ռ��CPUʱ��Ϊ��', F, 's')") time_end - time_begin
    
    close(1)
    !##############################################################�����н�����
    
    end subroutine elgauss
    
    !�������Ǿ���Ļش����㷽��A_up * x = b_up
    subroutine up_tri(A_up, b_up, x, n)
    !****************************************************************************************************
    !���������input����
    !1. A_up��ϵ������A��Ԫ�õ�����������
    !2. b_up����Ԫ�õ��ĳ�������b_up
    !3. x����Ž������
    !3. n�������ά��
    !���������output����
    !1. inv_A������A�������
    !�½�������new����
    !
    !�����ӳ��򣨺�������
    !
    !�������ӳ��򣨺�������
    !
    !��д�ļ�����
    !
    !****************************************************************************************************
    !
    implicit none
    
    !��������ά��n
    integer(kind=4) n
    
    !����ѭ������
    integer(kind=2) i, j
    
    !������Ԫ�õ�����������
    real(kind=8) A_up(n,n)
    !������Ԫ�õ��ĳ�������
    real(kind=8) b_up(n)
    !���������
    real(kind=8) x(n)
    
    !##############################################################�����п�ʼ��
    !���岢�м������
    integer(kind=4) nthreads, tid, omp_get_num_threads, omp_get_thread_num, chunksize, chunk
    parameter (chunksize = 100)
    !����CPUռ�ü�ʱ����
    real(kind=8) time_begin, time_end
    
    open(1, file = 'time_up_tri.txt')
    
    !CPU��ʱ���򣨼�ʱ��ʼ��
    call CPU_TIME(time_begin)
    
    chunk = chunksize
    
    !openMP��ʼ
    !$omp parallel shared(x, A_up, b_up, nthreads) private(i,j)
    !##############################################################�����п�ʼ��
    
    !�����x(n)
    x(n) = b_up(n) / A_up(n,n)
    
    !��ʼ�ش�
    !�ӵ�n-1�лش�����һ�У�����Ϊ-1
    do i = n-1, 1, -1
        !
        x(i) = b_up(i)
        do j = i+1, n
            x(i) = x(i) - A_up(i,j) * x(j)
        enddo
        !����x(i)ǰ���ϵ�����õ����ս�x(i)
        x(i) = x(i) / A_up(i,i)
    enddo
    !
    !write x
    
    !##############################################################�����н�����
    !openMP����
    !$omp end parallel
    
    !CPU��ʱ���򣨼�ʱ������
    call CPU_TIME(time_end)
    
    !���CPUռ��ʱ��
    write(1,"(1X, 'up_tri�ӳ����ܹ�ռ��CPUʱ��Ϊ��', F, 's')") time_end - time_begin
    
    close(1)
    !##############################################################�����н�����
    
    end subroutine up_tri
    
end module