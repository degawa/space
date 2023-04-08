program check
    use, intrinsic :: iso_fortran_env
    use :: space_axis
    use :: space_Cartesian_2d
    use :: fassert
    implicit none

    type(Cartesian_2d_type) :: space
    type(space_axis_type) :: ax

    space = new_Cartesian_2d([-1d0, 2d0], [-2d0, 3d0])

    call assert_equal(space%get_coordinate_values(), [-1d0, -2d0, 2d0, 3d0], &
                      "get_coordinate_values should be [-1d0, -2d0, 2d0, 3d0]")

    call assert_equal(space%get_length(), [3d0, 5d0], &
                      "get_length() should return [3, 5]")
    call assert_equal(space%get_length(x_dir_index), 3d0, &
                      "get_length(x) should return 3")
    call assert_equal(space%get_length(y_dir_index), 5d0, &
                      "get_length(y) should return 5")

    ax = space%get_axes(dim=x_dir_index)
    call assert_equal(ax%get_coord_values(), [-1d0, 2d0], &
                      "axis coord val should be [-1d0, 2d0]")
    ax = space%get_axes(dim=y_dir_index)
    call assert_equal(ax%get_coord_values(), [-2d0, 3d0], &
                      "axis coord val should be [-2d0, 3d0]")
end program check
