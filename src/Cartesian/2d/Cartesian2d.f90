!>デカルト座標系を表す派生型を提供する．
!>
module space_Cartesian_2d
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: iso_c_binding
    use :: space_axis, only:space_axis_type
    implicit none
    private
    public :: x_dir_index, y_dir_index
    public :: x_min_index, x_max_index, &
              y_min_index, y_max_index
    public :: xx_index, xy_index, yx_index, yy_index
    public :: new_Cartesian_2d

    integer(int32), private, parameter :: spatial_dimension = 2

    enum, bind(c)
        enumerator :: x_dir_index = 1
            !! デカルト座標系の軸方向成分\(x\)を参照するための配列添字
        enumerator :: y_dir_index
            !! デカルト座標系の軸方向成分\(y\)を参照するための配列添字
    end enum

    enum, bind(c)
        enumerator :: xx_index = 1
            !! デカルト座標系のテンソル量の成分\(xx\)を参照するための配列添字
        enumerator :: xy_index
            !! デカルト座標系のテンソル量の成分\(xy\)を参照するための配列添字
        enumerator :: yx_index
            !! デカルト座標系のテンソル量の成分\(yx\)を参照するための配列添字
        enumerator :: yy_index
            !! デカルト座標系のテンソル量の成分\(yy\)を参照するための配列添字
    end enum

    enum, bind(c)
        enumerator :: x_min_index = 1
            !! デカルト座標系の\(x\)軸の最小値を参照するための配列添字
        enumerator :: y_min_index
            !! デカルト座標系の\(y\)軸の最小値を参照するための配列添字
        enumerator :: x_max_index
            !! デカルト座標系の\(x\)軸の最大値を参照するための配列添字
        enumerator :: y_max_index
            !! デカルト座標系の\(y\)軸の最大値を参照するための配列添字
    end enum

    !>2次元デカルト座標系
    type, public :: Cartesian_2d_type
        type(space_axis_type), private :: x
            !! \(x\)座標
        type(space_axis_type), private :: y
            !! \(y\)座標
    contains
        procedure, public, pass :: set_coordinate_2d_by_array
        !* 配列の値に基づいて座標系の各軸を設定
        procedure, public, pass :: set_coodinate_2d_by_axis
        !* space_axis型変数に基づいて座標系の各軸を設定
        !&<
        generic :: construct => set_coordinate_2d_by_array, &
                                set_coodinate_2d_by_axis
        !&>

        procedure, public, pass :: get_coordinate_values => get_coordinate_2d
        !* 座標系の各軸の最小値と最大値を配列で返す<br>
        ! `=[x_min, y_min, x_max, y_max]`
        procedure, public, pass :: get_length_2d
        !* 座標系の各軸の長さを配列で返す<br>
        ! `=[length_x length_y]`
        procedure, public, pass :: get_length_2d_component
        !* 引数で指定した次元の軸の長さを返す
        procedure, public, pass :: get_axes_2d
        !* 各軸の情報を返す．
        procedure, public, pass :: get_axes_2d_component
        !* 引数で指定した軸の情報を返す．
        generic :: get_length => get_length_2d, get_length_2d_component
        generic :: get_axes => get_axes_2d, get_axes_2d_component

        procedure, public, pass :: assign_Cartesian_2d_type
        !* デカルト座標系を代入する
        generic :: assignment(=) => assign_Cartesian_2d_type
    end type Cartesian_2d_type

    interface new_Cartesian_2d
        procedure :: new_Cartesian_2d_axis
        procedure :: new_Cartesian_2d_coord_vals
    end interface
contains
    !>軸の値に基づいて2次元デカルト座標系型変数を設定する．
    function new_Cartesian_2d_axis(x_axis, y_axis) result(cart)
        implicit none
        type(space_axis_type), intent(in) :: x_axis
            !! space_axis型で表された\(x\)軸
        type(space_axis_type), intent(in) :: y_axis
            !! space_axis型で表された\(y\)軸
        type(Cartesian_2d_type) :: cart
            !! 1次元デカルト座標

        call cart%construct(x_axis, y_axis)
    end function new_Cartesian_2d_axis

    !>配列の値に基づいて1次元デカルト座標系型変数を設定する．
    function new_Cartesian_2d_coord_vals(x_coord_vals, y_coord_vals) result(cart)
        implicit none
        real(real64), intent(in) :: x_coord_vals(2)
            !! space_axis型で表された\(x\)軸
        real(real64), intent(in) :: y_coord_vals(2)
            !! space_axis型で表された\(y\)軸
        type(Cartesian_2d_type) :: cart
            !! 1次元デカルト座標

        call cart%construct(x_coord_vals, y_coord_vals)
    end function new_Cartesian_2d_coord_vals

    !>配列の値に基づいて2次元デカルト座標系型変数を設定する．
    subroutine set_coordinate_2d_by_array(this, x_coord_vals, y_coord_vals)
        implicit none
        !&<
        class(Cartesian_2d_type)  , intent(inout) :: this
            !! 2次元デカルト座標型の当該実体仮引数
        real(real64)              , intent(in)    :: x_coord_vals(2)
            !! \(x\)軸の最小値と最大値<br> `[min, max]`の順に格納
        real(real64)              , intent(in)    :: y_coord_vals(2)
            !! \(y\)軸の最小値と最大値<br> `[min, max]`の順に格納
        !&>

        this%x = x_coord_vals(:)
        this%y = y_coord_vals(:)
    end subroutine set_coordinate_2d_by_array

    !>軸の値に基づいて2次元デカルト座標系型変数を設定する．
    subroutine set_coodinate_2d_by_axis(this, x_axis, y_axis)
        implicit none
        !&<
        class(Cartesian_2d_type), intent(inout) :: this
            !! 2次元デカルト座標型の当該実体仮引数
        type(space_axis_type)   , intent(in)    :: x_axis
            !! space_axis型で表された\(x\)軸
        type(space_axis_type)   , intent(in)    :: y_axis
            !! space_axis型で表された\(y\)軸
        !&>

        this%x = x_axis
        this%y = y_axis
    end subroutine set_coodinate_2d_by_axis

    !>2次元デカルト座標系の各軸の座標値を取得する．
    function get_coordinate_2d(this) result(coord_vals)
        implicit none
        class(Cartesian_2d_type), intent(in) :: this
            !! 2次元デカルト座標型の当該実体仮引数

        real(real64) :: coord_vals(spatial_dimension*2)
            !! 各軸の座標値`=[x_min, y_min, x_max, y_max]`

        coord_vals(x_min_index) = this%x%get_coord_values("min")
        coord_vals(x_max_index) = this%x%get_coord_values("max")
        coord_vals(y_min_index) = this%y%get_coord_values("min")
        coord_vals(y_max_index) = this%y%get_coord_values("max")
    end function get_coordinate_2d

    !>2次元デカルト座標系の各軸の長さを取得する．
    function get_length_2d(this) result(lengths)
        implicit none
        class(Cartesian_2d_type), intent(in) :: this
            !! 2次元デカルト座標型の当該実体仮引数

        real(real64) :: lengths(spatial_dimension)
            !! 各軸の長さ`=[length_x, length_y]`

        lengths = [this%x%get_length(), &
                   this%y%get_length()]
    end function get_length_2d

    !>`dim`で指定した次元の各軸の長さを取得する．
    function get_length_2d_component(this, dim) result(length)
        implicit none
        class(Cartesian_2d_type), intent(in) :: this
            !! 2次元デカルト座標型の当該実体仮引数
        integer(int32), intent(in) :: dim
            !! 次元

        real(real64) :: length
            !! 軸の長さ

        select case (dim)
        case (x_dir_index)
            length = this%x%get_length()
        case (y_dir_index)
            length = this%y%get_length()
        end select
    end function get_length_2d_component

    !>2次元デカルト座標系の各軸を配列で返す
    function get_axes_2d(this) result(axes)
        implicit none
        class(Cartesian_2d_type), intent(in) :: this
            !! 当該実体仮引数

        type(space_axis_type) :: axes(spatial_dimension)
            !! 各軸情報

        axes(x_dir_index) = this%x
        axes(y_dir_index) = this%y
    end function get_axes_2d

    !>`dim`で指定した次元の軸を返す．
    function get_axes_2d_component(this, dim) result(ax)
        implicit none
        !&<
        class(Cartesian_2d_type), intent(in) :: this
            !! 当該実体仮引数
        integer(int32)          , intent(in) :: dim
        !&>

        type(space_axis_type) :: ax
            !! 各軸情報

        select case (dim)
        case (x_dir_index)
            ax = this%x
        case (y_dir_index)
            ax = this%y
        end select
    end function get_axes_2d_component

    !>2次元デカルト座標系型を代入する
    subroutine assign_Cartesian_2d_type(lhs, rhs)
        implicit none
        !&<
        class(Cartesian_2d_type), intent(inout) :: lhs
            !! 代入演算子左型
        class(Cartesian_2d_type), intent(in)    :: rhs
            !! 代入演算子右側
        !&>

        lhs%x = rhs%x
        lhs%y = rhs%y
    end subroutine assign_Cartesian_2d_type
end module space_Cartesian_2d
