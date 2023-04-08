program check
    use, intrinsic :: iso_fortran_env
    use :: space_axis
    use :: space_Cartesian_1d
    use :: fassert
    implicit none

    type(Cartesian_1d_type) :: space
    type(space_axis_type) :: ax

    space = new_Cartesian_1d([-1d0, 2d0])

    call assert_equal(space%get_coordinate_values(), [-1d0, 2d0], &
                      "get_coordinate_values should be [-1d0, 2d0]")

    call assert_equal(space%get_length(), [3d0], &
                      "get_length() should return 3")
    call assert_equal(space%get_length(x_dir_index), 3d0, &
                      "get_length(x) should return 3")

    ax = space%get_axes(dim=x_dir_index)
    call assert_equal(ax%get_coord_values(), [-1d0, 2d0], &
                      "axis coord val should be [-1d0, 2d0]")
end program check
