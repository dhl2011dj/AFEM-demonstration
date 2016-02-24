!****************************************************************************************************
!����ԭ������Ԫ���ӳ���������������C��Siԭ��λ�����ݲ��������ļ��������ʽ���������£�
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
subroutine initial_nodes_form(number_x, number_y, number_z, number_total, number_real, position, center_hole, radius_hole)

!ʹ��afem_parameterģ�飨���ж����˲�����length_unit_cell��length_cut_off
use afem_parameter

!Ĭ���������Ҫ��������
implicit none

    !�������
    
    !����x��y��z�����Ͼ����ĸ���
    integer(kind=4),intent(in)::number_x, number_y, number_z
    !����ȫ�������ĸ���
    integer(kind=4),intent(in)::number_total
    !����ʵ�ʴ��ڵľ����ĸ���
    integer(kind=4),intent(out)::number_real
    !����׵����ĺͰ뾶
    real(kind=8),intent(in)::center_hole(3), radius_hole
    !����λ�ô洢����
    !position(1,:)�������нڵ��X����
    !position(2,:)�������нڵ��Y����
    !position(3,:)�������нڵ��Z����
    real(kind=8)::position(3,number_total)
    
    !����Ƕȴ洢���飨�ɱ����飩���о�L-J���Ʋ���Ҫʹ�ã�
    !integer(kind=4),allocatable:: angle(:,:)

    !����������������
    !real(kind=8),intent(in)::center_all_nodes(3)
    real(kind=8) center_all_nodes(3)
    !����x��y��z�����ϵ����
    integer(kind=4) i_x, i_y, i_z
    !
    integer(kind=4) label_x, label_y, label_z
    !�������������ܺ�
    real(kind=8) total_xyz_all_nodes(3)
    !ѭ������
    integer(kind=4) i, j
    !��������֮�����ĺ���
    real(kind=8),external::distance
    !��������ڽڵ�����ʱ�����Ӿ����Բ�ĵľ���
    real(kind=8) r
    !��Ҫ��������ǣ��Ľڵ��
    real(kind=8) node_clear
    !real(kind=8) length_unit_cell = 0.54305    
    label_z = 0
    
    !����ѭ����������λ��
    !��һ��ѭ����i_x��i_yΪ������ż����Ӧ������Z�����ϲ�ͬ�����ӷֲ���ʽ
    !1==============================================================================
    do i_x = 1, number_x
        !����ѭ����label_x������ͬ��ֵ
        label_x = label_z
        
        !���i_xΪż����i_yΪ������ż��ʱ��Ӧ������Z�����ϲ�ͬ�����ӷֲ���ʽ
        if(mod(i_x, 2) == 0) then
            
            !�ڶ���ѭ����i_yΪż��,��ôZ����������λ�û��������
            !2======================================================================
            do i_y = 1, number_y
                !����ѭ����ÿ����ͬ��label_y��Ӧ��һ��ѭ����label_z
                label_y = label_z
                
                !���i_yΪż��,��ôZ����������λ�û��������
                if(mod(i_y, 2) == 0) then
                    
                    !������ѭ����Z����������λ�û��������
                    !3==============================================================
                    do i_z = 1, number_z
                        if(mod(i_z, 2) /= 0) then
                            !ÿ����һ�����ӵ�λ�ã�label_z����һ��
                            label_z = label_z + 1
                            !�����ӵ�Z�������position(3,:)����
                            position(3, label_z) = length_unit_cell / 2.0 * dble(i_z-1)
                        end if
                    end do
                    !3==============================================================
                    
                !���i_yΪ����,��ôZ������ż��λ�û��������
                else if(mod(i_y, 2) /= 0) then
                    
                    !������ѭ����Z������ż��λ�û��������
                    !3==============================================================
                    do i_z = 1, number_z
                        if(mod(i_z, 2) == 0) then
                            !ÿ����һ�����ӵ�λ�ã�label_z����һ��
                            label_z = label_z + 1
                            !�����ӵ�Z�������position(3,:)����
                            position(3, label_z) = length_unit_cell / 2.0 * dble(i_z-1)
                        end if
                    end do
                    !3==============================================================
                    
                end if
                !����label_y - label_z��ÿ��label_y��ͬ�����ӽ���������ֵ
                position(2, label_y+1:label_z) = length_unit_cell / 2 * dble(i_y-1)
            end do
            !2======================================================================
            
        !���i_xΪ������i_yΪ������ż��ʱ��Ӧ������Z�����ϲ�ͬ�����ӷֲ���ʽ
        else if(mod(i_x, 2) /= 0) then
            
            !�ڶ���ѭ����i_yΪż��,��ôZ������ż��λ�û��������
            !2======================================================================
            do i_y = 1, number_y
                !����ѭ����ÿ����ͬ��label_y��Ӧ��һ��ѭ����label_z
                label_y = label_z
                
                !���i_yΪż��,��ôZ������ż��λ�û��������
                if(mod(i_y, 2) == 0) then
                    
                    !������ѭ����Z������ż��λ�û��������
                    !3==============================================================
                    do i_z = 1, number_z
                        if(mod(i_z, 2) == 0) then
                            !ÿ����һ�����ӵ�λ�ã�label_z����һ��
                            label_z = label_z + 1
                            !�����ӵ�Z�������position(3,:)����
                            position(3, label_z) = length_unit_cell / 2.0 * dble(i_z-1)
                        end if
                    end do
                    !3==============================================================
                
                !���i_yΪ����,��ôZ����������λ�û��������
                else if(mod(i_y, 2) /= 0) then
                    
                    !������ѭ����Z����������λ�û��������
                    !3==============================================================
                    do i_z = 1, number_z
                        if(mod(i_z, 2) /= 0) then
                            !ÿ����һ�����ӵ�λ�ã�label_z����һ��
                            label_z = label_z + 1
                            !�����ӵ�Z�������position(3,:)����
                            position(3, label_z) = length_unit_cell / 2.0 * dble(i_z-1)
                        end if
                    end do
                    !3==============================================================
                    
                end if
                !����label_y - label_z��ÿ��label_y��ͬ�����ӽ���������ֵ
                position(2, label_y+1 : label_z) = length_unit_cell / 2 * dble(i_y-1)
            end do
            !2======================================================================
            
        end if
        !����label_x - label_z��ÿ��label_x��ͬ�����ӽ���������ֵ
        position(1, label_x+1 : label_z) = length_unit_cell / 2 * dble(i_x-1)
        
    !*******************************************************************************
        if(i_x < number_x) then
            label_x = label_z
            !���i_xΪż����i_yΪ������ż��ʱ��Ӧ������Z�����ϲ�ͬ�����ӷֲ���ʽ
            if(mod(i_x, 2) == 0) then
            
                !�ڶ���ѭ����i_yΪż��,��ôZ����������λ�û��������
                !2======================================================================
                do i_y = 1, number_y - 1
                    !����ѭ����ÿ����ͬ��label_y��Ӧ��һ��ѭ����label_z
                    label_y = label_z
                
                    !���i_yΪż��,��ôZ����������λ�û��������
                    if(mod(i_y, 2) == 0) then
                    
                        !������ѭ����Z����������λ�û��������
                        !3==============================================================
                        do i_z = 1, number_z - 1
                            if(mod(i_z, 2) /= 0) then
                                !ÿ����һ�����ӵ�λ�ã�label_z����һ��
                                label_z = label_z + 1
                                !�����ӵ�Z�������position(3,:)����
                                position(3, label_z) = length_unit_cell / 2.0 * dble(i_z-1) + length_unit_cell / 4.0
                            end if
                        end do
                        !3==============================================================
                    
                    !���i_yΪ����,��ôZ������ż��λ�û��������
                    else if(mod(i_y, 2) /= 0) then
                    
                        !������ѭ����Z������ż��λ�û��������
                        !3==============================================================
                        do i_z = 1, number_z - 1
                            if(mod(i_z, 2) == 0) then
                                !ÿ����һ�����ӵ�λ�ã�label_z����һ��
                                label_z = label_z + 1
                                !�����ӵ�Z�������position(3,:)����
                                position(3, label_z) = length_unit_cell / 2.0 * dble(i_z-1) + length_unit_cell / 4.0
                            end if
                        end do
                        !3==============================================================
                    
                    end if
                    !����label_y - label_z��ÿ��label_y��ͬ�����ӽ���������ֵ
                    position(2, label_y+1:label_z) = length_unit_cell / 2 * dble(i_y-1) + length_unit_cell / 4.0
                end do
                !2======================================================================
            
            !���i_xΪ������i_yΪ������ż��ʱ��Ӧ������Z�����ϲ�ͬ�����ӷֲ���ʽ
            else if(mod(i_x, 2) /= 0) then
            
                !�ڶ���ѭ����i_yΪż��,��ôZ������ż��λ�û��������
                !2======================================================================
                do i_y = 1, number_y - 1
                    !����ѭ����ÿ����ͬ��label_y��Ӧ��һ��ѭ����label_z
                    label_y = label_z
                
                    !���i_yΪż��,��ôZ������ż��λ�û��������
                    if(mod(i_y, 2) == 0) then
                    
                        !������ѭ����Z������ż��λ�û��������
                        !3==============================================================
                        do i_z = 1, number_z - 1
                            if(mod(i_z, 2) == 0) then
                                !ÿ����һ�����ӵ�λ�ã�label_z����һ��
                                label_z = label_z + 1
                                !�����ӵ�Z�������position(3,:)����
                                position(3, label_z) = length_unit_cell / 2.0 * dble(i_z-1) + length_unit_cell / 4.0
                            end if
                        end do
                        !3==============================================================
                
                    !���i_yΪ����,��ôZ����������λ�û��������
                    else if(mod(i_y, 2) /= 0) then
                    
                        !������ѭ����Z����������λ�û��������
                        !3==============================================================
                        do i_z = 1, number_z - 1
                            if(mod(i_z, 2) /= 0) then
                                !ÿ����һ�����ӵ�λ�ã�label_z����һ��
                                label_z = label_z + 1
                                !�����ӵ�Z�������position(3,:)����
                                position(3, label_z) = length_unit_cell / 2.0 * dble(i_z-1) + length_unit_cell / 4.0
                            end if
                        end do
                        !3==============================================================
                    
                    end if
                    !����label_y - label_z��ÿ��label_y��ͬ�����ӽ���������ֵ
                    position(2, label_y+1 : label_z) = length_unit_cell / 2 * dble(i_y-1) + length_unit_cell / 4.0
                end do
                !2======================================================================
            
            end if
            !����label_x - label_z��ÿ��label_x��ͬ�����ӽ���������ֵ
            position(1, label_x+1 : label_z) = length_unit_cell / 2 * dble(i_x-1) + length_unit_cell / 4.0

        end if
    
    end do
    !1============================================================================== 
    
    
    !����ǰ���label_z�õ�ʵ�ʵ�������
    number_real = label_z
    write(*,"(1X,'ȥ������Բ���е�����ǰ��������number_real = ',I10)") number_real
    !write(*,*) position(:,1:number_real)
    !�����ܺ�����
    total_xyz_all_nodes(1) = 0.0
    total_xyz_all_nodes(2) = 0.0
    total_xyz_all_nodes(3) = 0.0
    
    !���������ӵ�X��Y��Z�������������λ��
    do i = 1, number_real
        total_xyz_all_nodes(1) = total_xyz_all_nodes(1) + position(1,i)
        total_xyz_all_nodes(2) = total_xyz_all_nodes(2) + position(2,i)
        total_xyz_all_nodes(3) = total_xyz_all_nodes(3) + position(3,i)
    end do
    center_all_nodes(1) = total_xyz_all_nodes(1) / dble(number_real)
    center_all_nodes(2) = total_xyz_all_nodes(2) / dble(number_real)
    center_all_nodes(3) = total_xyz_all_nodes(3) / dble(number_real)
    
    !����������������ã�����ԭ������������
    !do i = 1, number_real
    !    position(1,i) = position(1,i) - center_all_nodes(1)
    !    position(2,i) = position(2,i) - center_all_nodes(2)
    !    position(3,i) = position(3,i) - center_all_nodes(3)
    !end do
    
    
    !ȥ�����ڵĽڵ�
    !������������ڿ���Ļ���i��˳�����Ӷ�cû������
    !Ȼ������i��������ǰ��������c������
    node_clear = 0
    do i = 1, number_real
        r = distance(position(:,i), center_hole)
        if(r >= radius_hole) then
            node_clear = node_clear + 1
            !��������������������ǰ����
            do j = 1, 3
                position(j, node_clear)=position(j, i)
            enddo
        endif
    end do
    
    !�������������ӵĸ�������number_real
    number_real = node_clear
    write(*,"(1X,'ȥ������Բ���е����Ӻ�������number_real = ',I10)") number_real
    
end