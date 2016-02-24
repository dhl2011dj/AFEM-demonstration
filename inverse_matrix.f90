!****************************************************************************************************
!这是原子有限元的模块，作用是使用高斯列主元消去法计算：
!1. inv_matrix(A, inv_A, n)：计算矩阵A的逆矩阵
!2. matrix_gauss(A, B, X, n, m)：计算有限元方程
!****************************************************************************************************
module inverse_matrix
contains
    !计算矩阵A的逆矩阵
    subroutine inv_matrix(A, inv_A, n)
    !****************************************************************************************************
    !读入变量（input）：
    !1. A：需要求逆的方阵
    !2. n：方阵的维度
    !输出变量（output）：
    !1. inv_A：方阵A的逆矩阵
    !新建变量（new）：
    !
    !调用子程序（函数）：
    !
    !被调用子程序（函数）：
    !
    !读写文件名：
    !
    !****************************************************************************************************
    implicit none
    
    !定义方阵的维度n
    integer(kind=4) n
    
    !定义循环变量
    integer(kind=2) i,j
    
    !矩阵A
    real(kind=8) A(n,n)
    !逆矩阵inv_A
    real(kind=8) inv_A(n,n)
    !单位矩阵
    real(kind=8) E(n,n)
    
    !赋初始值
    E = 0.0
    do i = 1, n
        !给单位矩阵赋值
        E(i,i) = 1.0
    enddo
    
    call matrix_gauss(A, E, inv_A, n, n)
    
    !输出系数矩阵A的逆矩阵inv_A
    !write(*,"(/,1X, 'inv_A = ')")
    !write(*,"(1X, 4F8.4)")((inv_A(i,j),j = 1,n),i = 1,n)
    
    end subroutine inv_matrix
    
    !利用Gauss消去法计算矩阵方程（将m个常数向量分开计算）
    subroutine matrix_gauss(A, B, X, n, m)
    !****************************************************************************************************
    !读入变量（input）：
    !1. A：需要求逆的方阵
    !2. n：方阵的维度
    !3. B：n行m列常数矩阵
    !4. m：常数矩阵B的列数，如果B是列向量的话，m = 1
    !5. X：n行m列的解矩阵，如果B是列向量即m = 1的话，X为解向量
    !输出变量（output）：
    !1. X：n行m列的解矩阵，如果B是列向量即m = 1的话，X为解向量
    !新建变量（new）：
    !
    !调用子程序（函数）：
    !
    !被调用子程序（函数）：
    !
    !读写文件名：
    !
    !****************************************************************************************************
    implicit none
    
    !定义矩阵的维度n
    integer(kind=4) n, m
    
    !定义循环变量
    integer(kind=2) i, j
    
    !矩阵A
    real(kind=8) A(n,n)
    !
    real(kind=8) B(n,m)
    !
    real(kind=8) X(n,m)
    !
    real(kind=8) X_temp(n), b_temp(n)
    
    !##############################################################程序并行开始块
    !定义并行计算变量
    integer(kind=4) nthreads, tid, omp_get_num_threads, omp_get_thread_num, chunksize, chunk
    parameter (chunksize = 100)
    !定义CPU占用计时变量
    real(kind=8) time_begin, time_end
    
    open(1, file = 'time_matrix_gauss.txt')
    
    !CPU计时程序（计时开始）
    call CPU_TIME(time_begin)
    
    chunk = chunksize
    
    !openMP开始
    !$omp parallel shared(A, B, X, X_temp, b_temp) private(i,j)
    !##############################################################程序并行开始块
    
    do i = 1, m
        
        !将B按列放入Btemp中以进行求解
        b_temp(:) = B(: , i)
        
        call elgauss(A, b_temp, X_temp, n)
        !将求解得到的Xtemp按列放入X中
        X(:,i) = X_temp
        
    enddo
    
    !##############################################################程序并行结束块
    !openMP结束
    !$omp end parallel
    
    !CPU计时程序（计时结束）
    call CPU_TIME(time_end)
    
    !输出CPU占用时间
    write(1,"(1X, 'matrix_gauss子程序总共占用CPU时间为：', F, 's')") time_end - time_begin
    
    close(1)
    !##############################################################程序并行结束块
    !输出解矩阵X
    !write(*,"(/,1X, '解矩阵：X = ')")
    !write(*,"(1X, 4F8.4)")((X(i,j),j = 1,m),i = 1,n)
    
    end subroutine matrix_gauss
    
    !Gauss列主元消去法（将系数矩阵消元成为上三角阵）
    subroutine elgauss(A, b, x, n)
    !****************************************************************************************************
    !读入变量（input）：
    !1. A：需要求逆的方阵
    !2. n：方阵的维度
    !输出变量（output）：
    !1. inv_A：方阵A的逆矩阵
    !新建变量（new）：
    !
    !调用子程序（函数）：
    !
    !被调用子程序（函数）：
    !
    !读写文件名：
    !
    !****************************************************************************************************
    !
    implicit none
    
    !定义矩阵的维度n
    integer(kind=4) n
    
    !主元素编号
    integer(kind=4) id_max
    
    !定义循环变量
    integer(kind=2) i, k
    
    !列主元（每列中最大的数）
    real(kind=8) element_max
    !矩阵A
    real(kind=8) A(n,n)
    !单列的常数向量
    real(kind=8) b(n)
    !
    real(kind=8) x(n)
    
    real(kind=8) A_up(n,n), b_up(n)
    !增广矩阵[Ab]
    real(kind=8) Ab(n,n+1)
    !
    real(kind=8) V_temp1(n+1), V_temp2(n+1)
    !列主元所在列对列主元的比例因子
    real(kind=8) factor
    
    !##############################################################程序并行开始块
    !定义并行计算变量
    integer(kind=4) nthreads, tid, omp_get_num_threads, omp_get_thread_num, chunksize, chunk
    parameter (chunksize = 100)
    !定义CPU占用计时变量
    real(kind=8) time_begin, time_end
    
    open(1, file = 'time_elgauss.txt')
    
    !CPU计时程序（计时开始）
    call CPU_TIME(time_begin)
    
    chunk = chunksize
    
    !openMP开始
    !$omp parallel shared(element_max, A, b, x, A_up, b_up, Ab, V_temp1, V_temp2) private(i,j)
    !##############################################################程序并行开始块
    
    !将A矩阵和b向量集成到增广矩阵中去
    Ab(1:n,1:n) = A
    Ab(:,n+1) = b
    
    !进行列主元的选取并交换k行和i行元素的位置
    do k = 1, n-1
        !选取列主元及其相应的行号，放入element_max和id_max
        element_max = dabs(Ab(k,k))
        id_max = k
        do i = k+1, n
            if(dabs(Ab(i,k)).GT.element_max) then
                element_max = Ab(i,k)
                id_max = i
            endif
        enddo
        !交换k行元素和列主元所在行的元素
        V_temp1 = Ab(k,:)
        V_temp2 = Ab(id_max,:)
        Ab(k,:) = V_temp2
        Ab(id_max,:) = V_temp1
        
        
        do i = k+1, n
            !归一化
            factor = Ab(i,k) / Ab(k,k)
            !消元
            Ab(i,:) = Ab(i,:) - factor * Ab(k,:)
        enddo
    enddo
    
    A_up(:,:) = Ab(1:n,1:n)
    b_up(:) = Ab(:,n+1)
    
    call up_tri(A_up, b_up, x, n)
    
    !##############################################################程序并行结束块
    !openMP结束
    !$omp end parallel
    
    !CPU计时程序（计时结束）
    call CPU_TIME(time_end)
    
    !输出CPU占用时间
    write(1,"(1X, 'elgauss子程序总共占用CPU时间为：', F, 's')") time_end - time_begin
    
    close(1)
    !##############################################################程序并行结束块
    
    end subroutine elgauss
    
    !将上三角矩阵的回带计算方程A_up * x = b_up
    subroutine up_tri(A_up, b_up, x, n)
    !****************************************************************************************************
    !读入变量（input）：
    !1. A_up：系数矩阵A消元得到的上三角阵
    !2. b_up：消元得到的常数向量b_up
    !3. x：存放解的向量
    !3. n：方阵的维度
    !输出变量（output）：
    !1. inv_A：方阵A的逆矩阵
    !新建变量（new）：
    !
    !调用子程序（函数）：
    !
    !被调用子程序（函数）：
    !
    !读写文件名：
    !
    !****************************************************************************************************
    !
    implicit none
    
    !定义矩阵的维度n
    integer(kind=4) n
    
    !定义循环变量
    integer(kind=2) i, j
    
    !定义消元得到的上三角阵
    real(kind=8) A_up(n,n)
    !定义消元得到的常数向量
    real(kind=8) b_up(n)
    !定义解向量
    real(kind=8) x(n)
    
    !##############################################################程序并行开始块
    !定义并行计算变量
    integer(kind=4) nthreads, tid, omp_get_num_threads, omp_get_thread_num, chunksize, chunk
    parameter (chunksize = 100)
    !定义CPU占用计时变量
    real(kind=8) time_begin, time_end
    
    open(1, file = 'time_up_tri.txt')
    
    !CPU计时程序（计时开始）
    call CPU_TIME(time_begin)
    
    chunk = chunksize
    
    !openMP开始
    !$omp parallel shared(x, A_up, b_up, nthreads) private(i,j)
    !##############################################################程序并行开始块
    
    !先求解x(n)
    x(n) = b_up(n) / A_up(n,n)
    
    !开始回带
    !从第n-1行回带到第一行，步长为-1
    do i = n-1, 1, -1
        !
        x(i) = b_up(i)
        do j = i+1, n
            x(i) = x(i) - A_up(i,j) * x(j)
        enddo
        !除以x(i)前面的系数即得到最终解x(i)
        x(i) = x(i) / A_up(i,i)
    enddo
    !
    !write x
    
    !##############################################################程序并行结束块
    !openMP结束
    !$omp end parallel
    
    !CPU计时程序（计时结束）
    call CPU_TIME(time_end)
    
    !输出CPU占用时间
    write(1,"(1X, 'up_tri子程序总共占用CPU时间为：', F, 's')") time_end - time_begin
    
    close(1)
    !##############################################################程序并行结束块
    
    end subroutine up_tri
    
end module