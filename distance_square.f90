!�����������Ǽ�������֮��ľ���
real(kind=8) function distance_square(x1,x2)
implicit none
	real(kind=8) x1(3), x2(3)
    !����
	distance_square = (x1(1)-x2(1))**2 + (x1(2)-x2(2))**2 + (x1(3)-x2(3))**2
return
end function distance_square