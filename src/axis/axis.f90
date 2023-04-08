!>座標系の各軸の情報（最小値と最大値）を取り扱う型を提供する．
!>
module space_axis
    use, intrinsic :: iso_fortran_env
    use :: axis
    implicit none
    private
    public :: new_space_axis

    type, public, extends(axis_type) :: space_axis_type
    end type space_axis_type

    interface new_space_axis
        procedure :: new_space_axis_from_array
        procedure :: new_space_axis_from_coord_vals
    end interface
contains
    !>引数で指定した軸の最小値，最大値に基づいてaxis型変数を生成する．
    function new_space_axis_from_coord_vals(min_coord_val, max_coord_val) result(ax)
        implicit none
        real(real64), intent(in) :: min_coord_val
            !! 軸の最小値
        real(real64), intent(in) :: max_coord_val
            !! 軸の最大値

        type(space_axis_type) :: ax

        call ax%construct(min_coord_val, max_coord_val)
    end function new_space_axis_from_coord_vals

    !>引数で指定した，軸の最小値，最大値を持つ配列に基づいて
    !>axis型変数を生成する．
    function new_space_axis_from_array(coord_vals) result(ax)
        implicit none
        real(real64), intent(in) :: coord_vals(2)
            !! 軸の最小値と最大値<br> `[min, max]`の順に格納

        type(space_axis_type) :: ax

        call ax%construct(coord_vals)
    end function new_space_axis_from_array
end module space_axis
