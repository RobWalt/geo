/// Creates a [`Point`] from the given coordinates.
///
/// ```txt
/// point! { x: <number>, y: <number> }
/// point!(<coordinate>)
/// ```
///
/// # Examples
///
/// Creating a [`Point`], supplying x/y values:
///
/// ```
/// use geo_types::{point, coord};
///
/// let p = point! { x: 181.2, y: 51.79 };
///
/// assert_eq!(p.x(), 181.2);
/// assert_eq!(p.y(), 51.79);
///
/// let p = point!(coord! { x: 181.2, y: 51.79 });
///
/// assert_eq!(p.x(), 181.2);
/// assert_eq!(p.y(), 51.79);
/// ```
///
/// [`Point`]: ./struct.Point.html
#[macro_export]
macro_rules! point {
    ( $($tag:tt : $val:expr),* $(,)? ) => {
        $crate::point! ( $crate::coord! { $( $tag: $val , )* } )
    };
    ( $coord:expr $(,)? ) => {
        $crate::Point::from($coord)
    };
}

/// Creates a [`Coord`] from the given scalars.
///
/// ```txt
/// coord! { x: <number>, y: <number> }
/// ```
///
/// # Examples
///
/// Creating a [`Coord`], supplying x/y values:
///
/// ```
/// use geo_types::coord;
///
/// let c = coord! { x: 181.2, y: 51.79 };
///
/// assert_eq!(c, geo_types::coord! { x: 181.2, y: 51.79 });
/// ```
///
/// [`Coord`]: ./struct.Coord.html
#[macro_export]
macro_rules! coord {
    (x: $x:expr, y: $y:expr $(,)* ) => {
        $crate::Coord { x: $x, y: $y }
    };
}

/// Creates a [`LineString`] containing the given coordinates.
///
/// ```txt
/// line_string![Coord OR (x: <number>, y: <number>), …]
/// ```
///
/// # Examples
///
/// Creating a [`LineString`], supplying x/y values:
///
/// ```
/// use geo_types::line_string;
///
/// let ls = line_string![
///     (x: -21.95156, y: 64.1446),
///     (x: -21.951, y: 64.14479),
///     (x: -21.95044, y: 64.14527),
///     (x: -21.951445, y: 64.145508),
/// ];
///
/// assert_eq!(ls[1], geo_types::coord! {
///     x: -21.951,
///     y: 64.14479
/// });
/// ```
///
/// Creating a [`LineString`], supplying [`Coord`]s:
///
/// ```
/// use geo_types::line_string;
///
/// let coord1 = geo_types::coord! {
///     x: -21.95156,
///     y: 64.1446,
/// };
/// let coord2 = geo_types::coord! {
///     x: -21.951,
///     y: 64.14479,
/// };
/// let coord3 = geo_types::coord! {
///     x: -21.95044,
///     y: 64.14527,
/// };
/// let coord4 = geo_types::coord! {
///     x: -21.951445,
///     y: 64.145508,
/// };
///
/// let ls = line_string![coord1, coord2, coord3, coord4];
///
/// assert_eq!(
///     ls[1],
///     geo_types::coord! {
///         x: -21.951,
///         y: 64.14479
///     }
/// );
/// ```
///
/// [`Coord`]: ./struct.Coord.html
/// [`LineString`]: ./line_string/struct.LineString.html
#[macro_export]
macro_rules! line_string {
    () => { $crate::LineString::new($crate::_alloc::vec![]) };
    (
        $(( $($tag:tt : $val:expr),* $(,)? )),*
        $(,)?
    ) => {
        line_string![
            $(
                $crate::coord! { $( $tag: $val , )* },
            )*
        ]
    };
    (
        $($coord:expr),*
        $(,)?
    ) => {
        $crate::LineString::new(
            $crate::_alloc::vec![
                $($coord),*
            ]
        )
    };
}

/// Creates a [`Polygon`] containing the given coordinates.
///
/// ```txt
/// polygon![Coord OR (x: <number>, y: <number>), …]
///
/// // or
///
/// polygon!(
///     exterior: [Coord OR (x: <number>, y: <number>), …],
///     interiors: [
///         [Coord OR (x: <number>, y: <number>), …],
///         …
///     ],
/// )
/// ```
///
/// # Examples
///
/// Creating a [`Polygon`] without interior rings, supplying x/y values:
///
/// ```
/// use geo_types::polygon;
///
/// let poly = polygon![
///     (x: -111., y: 45.),
///     (x: -111., y: 41.),
///     (x: -104., y: 41.),
///     (x: -104., y: 45.),
/// ];
///
/// assert_eq!(
///     poly.exterior()[1],
///     geo_types::coord! { x: -111., y: 41. },
/// );
/// ```
///
/// Creating a [`Polygon`], supplying x/y values:
///
/// ```
/// use geo_types::polygon;
///
/// let poly = polygon!(
///     exterior: [
///         (x: -111., y: 45.),
///         (x: -111., y: 41.),
///         (x: -104., y: 41.),
///         (x: -104., y: 45.),
///     ],
///     interiors: [
///         [
///             (x: -110., y: 44.),
///             (x: -110., y: 42.),
///             (x: -105., y: 42.),
///             (x: -105., y: 44.),
///         ],
///     ],
/// );
///
/// assert_eq!(
///     poly.exterior()[1],
///     geo_types::coord! { x: -111., y: 41. },
/// );
/// ```
///
/// [`Coord`]: ./struct.Coord.html
/// [`Polygon`]: ./struct.Polygon.html
#[macro_export]
macro_rules! polygon {
    () => { $crate::Polygon::new($crate::line_string![], $crate::_alloc::vec![]) };
    (
        exterior: [
            $(( $($exterior_tag:tt : $exterior_val:expr),* $(,)? )),*
            $(,)?
        ],
        interiors: [
            $([
                $(( $($interior_tag:tt : $interior_val:expr),* $(,)? )),*
                $(,)?
            ]),*
            $(,)?
        ]
        $(,)?
    ) => {
        polygon!(
            exterior: [
                $(
                    $crate::coord! { $( $exterior_tag: $exterior_val , )* },
                )*
            ],
            interiors: [
                $([
                    $($crate::coord! { $( $interior_tag: $interior_val , )* }),*
                ]),*
            ],
        )
    };
    (
        exterior: [
            $($exterior_coord:expr),*
            $(,)?
        ],
        interiors: [
            $([
                $($interior_coord:expr),*
                $(,)?
            ]),*
            $(,)?
        ]
        $(,)?
    ) => {
        $crate::Polygon::new(
            $crate::line_string![
                $($exterior_coord), *
            ],
            $crate::_alloc::vec![
                $(
                    $crate::line_string![$($interior_coord),*]
                ), *
            ]
        )
    };
    (
        $(( $($tag:tt : $val:expr),* $(,)? )),*
        $(,)?
    ) => {
        polygon![
            $($crate::coord! { $( $tag: $val , )* }),*
        ]
    };
    (
        $($coord:expr),*
        $(,)?
    ) => {
        $crate::Polygon::new(
            $crate::line_string![$($coord,)*],
            $crate::_alloc::vec![],
        )
    };
}

/// Creates a [`crate::geometry`] from a
/// [WKT](https://en.wikipedia.org/wiki/Well-known_text_representation_of_geometry) literal.
///
/// This is evaluated at compile time, so you don't need to worry about runtime errors from inavlid
/// WKT syntax.
///
/// Note that `POINT EMPTY` is not accepted because it is not representable as a `geo_types::Point`.
///
/// ```
/// use geo_types::wkt;
/// let point = wkt! { POINT(1.0 2.0) };
/// assert_eq!(point.x(), 1.0);
/// assert_eq!(point.y(), 2.0);
///
/// let geometry_collection = wkt! {
///     GEOMETRYCOLLECTION(
///         POINT(1.0 2.0),
///         LINESTRING EMPTY,
///         POLYGON((0.0 0.0,1.0 0.0,1.0 1.0,0.0 0.0))
///     )
/// };
/// assert_eq!(geometry_collection.len(), 3);
/// ```
#[macro_export]
macro_rules! wkt {
    // Hide distracting implementation details from the generated rustdoc.
    ($($wkt:tt)+) => {
        {
            $crate::wkt_internal!($($wkt)+)
        }
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! wkt_internal {
    (POINT EMPTY) => {
        compile_error!("EMPTY points are not supported in geo-types")
    };
    (POINT($x: literal $y: literal)) => {
        $crate::point!(x: $x, y: $y)
    };
    (POINT $($tail: tt)*) => {
        compile_error!("Invalid POINT wkt");
    };
    (LINESTRING EMPTY) => {
        $crate::line_string![]
    };
    (LINESTRING ($($x: literal $y: literal),+)) => {
        $crate::line_string![
            $($crate::coord!(x: $x, y: $y)),*
        ]
    };
    (LINESTRING ()) => {
        compile_error!("use `EMPTY` instead of () for an empty collection")
    };
    (LINESTRING $($tail: tt)*) => {
        compile_error!("Invalid LINESTRING wkt");
    };
    (POLYGON EMPTY) => {
        $crate::polygon![]
    };
    (POLYGON ( $exterior_tt: tt )) => {
        $crate::Polygon::new($crate::wkt!(LINESTRING $exterior_tt), $crate::_alloc::vec![])
    };
    (POLYGON( $exterior_tt: tt, $($interiors_tt: tt),+ )) => {
        $crate::Polygon::new(
            $crate::wkt!(LINESTRING $exterior_tt),
            $crate::_alloc::vec![
               $($crate::wkt!(LINESTRING $interiors_tt)),*
            ]
        )
    };
    (POLYGON ()) => {
        compile_error!("use `EMPTY` instead of () for an empty collection")
    };
    (POLYGON $($tail: tt)*) => {
        compile_error!("Invalid POLYGON wkt");
    };
    (MULTIPOINT EMPTY) => {
        $crate::MultiPoint($crate::_alloc::vec![])
    };
    (MULTIPOINT ()) => {
        compile_error!("use `EMPTY` instead of () for an empty collection")
    };
    (MULTIPOINT ($($x: literal $y: literal),* )) => {
        $crate::MultiPoint(
            $crate::_alloc::vec![$($crate::point!(x: $x, y: $y)),*]
        )
    };
    (MULTIPOINT $($tail: tt)*) => {
        compile_error!("Invalid MULTIPOINT wkt");
    };
    (MULTILINESTRING EMPTY) => {
        $crate::MultiLineString($crate::_alloc::vec![])
    };
    (MULTILINESTRING ()) => {
        compile_error!("use `EMPTY` instead of () for an empty collection")
    };
    (MULTILINESTRING ( $($line_string_tt: tt),* )) => {
        $crate::MultiLineString($crate::_alloc::vec![
           $($crate::wkt!(LINESTRING $line_string_tt)),*
        ])
    };
    (MULTILINESTRING $($tail: tt)*) => {
        compile_error!("Invalid MULTILINESTRING wkt");
    };
    (MULTIPOLYGON EMPTY) => {
        $crate::MultiPolygon($crate::_alloc::vec![])
    };
    (MULTIPOLYGON ()) => {
        compile_error!("use `EMPTY` instead of () for an empty collection")
    };
    (MULTIPOLYGON ( $($polygon_tt: tt),* )) => {
        $crate::MultiPolygon($crate::_alloc::vec![
           $($crate::wkt!(POLYGON $polygon_tt)),*
        ])
    };
    (MULTIPOLYGON $($tail: tt)*) => {
        compile_error!("Invalid MULTIPOLYGON wkt");
    };
    (GEOMETRYCOLLECTION EMPTY) => {
        $crate::GeometryCollection($crate::_alloc::vec![])
    };
    (GEOMETRYCOLLECTION ()) => {
        compile_error!("use `EMPTY` instead of () for an empty collection")
    };
    (GEOMETRYCOLLECTION ( $($el_type:tt $el_tt: tt),* )) => {
        $crate::GeometryCollection($crate::_alloc::vec![
           $($crate::Geometry::from($crate::wkt!($el_type $el_tt))),*
        ])
    };
    (GEOMETRYCOLLECTION $($tail: tt)*) => {
        compile_error!("Invalid GEOMETRYCOLLECTION wkt");
    };
    ($name: ident ($($tail: tt)*)) => {
        compile_error!("Unknown type. Must be one of POINT, LINESTRING, POLYGON, MULTIPOINT, MULTILINESTRING, MULTIPOLYGON, or GEOMETRYCOLLECTION");
    };
}

#[cfg(test)]
mod test {
    use crate::geometry::*;
    use alloc::vec;

    #[test]
    fn test_point() {
        let p = point! { x: 1.2, y: 3.4 };
        assert_eq!(p.x(), 1.2);
        assert_eq!(p.y(), 3.4);

        let p = point! {
            x: 1.2,
            y: 3.4,
        };
        assert_eq!(p.x(), 1.2);
        assert_eq!(p.y(), 3.4);

        let p = point!(coord! { x: 1.2, y: 3.4 });
        assert_eq!(p.x(), 1.2);
        assert_eq!(p.y(), 3.4);

        let p = point!(coord! { x: 1.2, y: 3.4 },);
        assert_eq!(p.x(), 1.2);
        assert_eq!(p.y(), 3.4);
    }

    #[test]
    fn test_line() {
        let ls = line_string![(x: -1.2f32, y: 3.4f32)];
        assert_eq!(ls[0], coord! { x: -1.2, y: 3.4 });

        let ls = line_string![
            (x: -1.2f32, y: 3.4f32),
        ];
        assert_eq!(ls[0], coord! { x: -1.2, y: 3.4 });

        let ls = line_string![(
            x: -1.2f32,
            y: 3.4f32,
        )];
        assert_eq!(ls[0], coord! { x: -1.2, y: 3.4 });

        let ls = line_string![
            (x: -1.2f32, y: 3.4f32),
            (x: -5.6, y: 7.8),
        ];
        assert_eq!(ls[0], coord! { x: -1.2, y: 3.4 });
        assert_eq!(ls[1], coord! { x: -5.6, y: 7.8 });
    }

    #[test]
    fn test_polygon() {
        let p = polygon!(
            exterior: [(x: 1, y: 2)],
            interiors: [[(x: 3, y: 4)]]
        );
        assert_eq!(p.exterior()[0], coord! { x: 1, y: 2 });
        assert_eq!(p.interiors()[0][0], coord! { x: 3, y: 4 });

        let p = polygon!(
            exterior: [(x: 1, y: 2)],
            interiors: [[(x: 3, y: 4)]],
        );
        assert_eq!(p.exterior()[0], coord! { x: 1, y: 2 });
        assert_eq!(p.interiors()[0][0], coord! { x: 3, y: 4 });

        let p = polygon!(
            exterior: [(x: 1, y: 2, )],
            interiors: [[(x: 3, y: 4, )]],
        );
        assert_eq!(p.exterior()[0], coord! { x: 1, y: 2 });
        assert_eq!(p.interiors()[0][0], coord! { x: 3, y: 4 });

        let p = polygon!(
            exterior: [(x: 1, y: 2, ), ],
            interiors: [[(x: 3, y: 4, ), ]],
        );
        assert_eq!(p.exterior()[0], coord! { x: 1, y: 2 });
        assert_eq!(p.interiors()[0][0], coord! { x: 3, y: 4 });

        let p = polygon!(
            exterior: [(x: 1, y: 2, ), ],
            interiors: [[(x: 3, y: 4, ), ], ],
        );
        assert_eq!(p.exterior()[0], coord! { x: 1, y: 2 });
        assert_eq!(p.interiors()[0][0], coord! { x: 3, y: 4 });
    }

    #[test]
    fn point() {
        let point = wkt! { POINT(1.0 2.0) };
        assert_eq!(point.x(), 1.0);
        assert_eq!(point.y(), 2.0);

        let point = wkt! { POINT(1.0   2.0) };
        assert_eq!(point.x(), 1.0);
        assert_eq!(point.y(), 2.0);

        // This (rightfully) fails to compile because geo-types doesn't support "empty" points
        // wkt! { POINT EMPTY }
    }

    #[test]
    fn empty_line_string() {
        let line_string: LineString<f64> = wkt! { LINESTRING EMPTY };
        assert_eq!(line_string.0.len(), 0);

        // This (rightfully) fails to compile because its invalid wkt
        // wkt! { LINESTRING() }
    }

    #[test]
    fn line_string() {
        let line_string = wkt! { LINESTRING(1.0 2.0,3.0 4.0) };
        assert_eq!(line_string.0.len(), 2);
        assert_eq!(line_string[0], coord! { x: 1.0, y: 2.0 });
    }

    #[test]
    fn empty_polygon() {
        let polygon: Polygon = wkt! { POLYGON EMPTY };
        assert_eq!(polygon.exterior().0.len(), 0);
        assert_eq!(polygon.interiors().len(), 0);

        // This (rightfully) fails to compile because its invalid wkt
        // wkt! { POLYGON() }
    }

    #[test]
    fn polygon() {
        let polygon = wkt! { POLYGON((1.0 2.0)) };
        assert_eq!(polygon.exterior().0.len(), 1);
        assert_eq!(polygon.exterior().0[0], coord! { x: 1.0, y: 2.0 });

        let polygon = wkt! { POLYGON((1.0 2.0,3.0 4.0)) };
        // Note: an extra coord is added to close the linestring
        assert_eq!(polygon.exterior().0.len(), 3);
        assert_eq!(polygon.exterior().0[0], coord! { x: 1.0, y: 2.0 });
        assert_eq!(polygon.exterior().0[1], coord! { x: 3.0, y: 4.0 });
        assert_eq!(polygon.exterior().0[2], coord! { x: 1.0, y: 2.0 });

        let polygon = wkt! { POLYGON((1.0 2.0), (1.1 2.1)) };
        assert_eq!(polygon.exterior().0.len(), 1);
        assert_eq!(polygon.interiors().len(), 1);

        assert_eq!(polygon.exterior().0[0], coord! { x: 1.0, y: 2.0 });
        assert_eq!(polygon.interiors()[0].0[0], coord! { x: 1.1, y: 2.1 });

        let polygon = wkt! { POLYGON((1.0 2.0,3.0 4.0), (1.1 2.1,3.1 4.1), (1.2 2.2,3.2 4.2)) };
        assert_eq!(polygon.exterior().0.len(), 3);
        assert_eq!(polygon.interiors().len(), 2);
        assert_eq!(polygon.interiors()[1][1], coord! { x: 3.2, y: 4.2 });
    }

    #[test]
    fn empty_multi_point() {
        let multipoint: MultiPoint = wkt! { MULTIPOINT EMPTY };
        assert!(multipoint.0.is_empty());
        // This (rightfully) fails to compile because its invalid wkt
        // wkt! { MULTIPOINT() }
    }

    #[test]
    fn multi_point() {
        let multi_point = wkt! { MULTIPOINT(1.0 2.0) };
        assert_eq!(multi_point.0, vec![point! { x: 1.0, y: 2.0}]);

        let multi_point = wkt! { MULTIPOINT(1.0 2.0,3.0 4.0) };
        assert_eq!(
            multi_point.0,
            vec![point! { x: 1.0, y: 2.0}, point! { x: 3.0, y: 4.0}]
        );
    }

    #[test]
    fn empty_multi_line_string() {
        let multi_line_string: MultiLineString = wkt! { MULTILINESTRING EMPTY };
        assert_eq!(multi_line_string.0, vec![]);
        // This (rightfully) fails to compile because its invalid wkt
        // wkt! { MULTILINESTRING() }
    }
    #[test]
    fn multi_line_string() {
        let multi_line_string = wkt! { MULTILINESTRING ((1.0 2.0,3.0 4.0)) };
        assert_eq!(multi_line_string.0.len(), 1);
        assert_eq!(multi_line_string.0[0].0[1], coord! { x: 3.0, y: 4.0 });
        let multi_line_string = wkt! { MULTILINESTRING ((1.0 2.0,3.0 4.0),(5.0 6.0,7.0 8.0)) };
        assert_eq!(multi_line_string.0.len(), 2);
        assert_eq!(multi_line_string.0[1].0[1], coord! { x: 7.0, y: 8.0 });

        let multi_line_string = wkt! { MULTILINESTRING ((1.0 2.0,3.0 4.0),EMPTY) };
        assert_eq!(multi_line_string.0.len(), 2);
        assert_eq!(multi_line_string.0[1].0.len(), 0);
    }

    #[test]
    fn empty_multi_polygon() {
        let multi_polygon: MultiPolygon = wkt! { MULTIPOLYGON EMPTY };
        assert!(multi_polygon.0.is_empty());

        // This (rightfully) fails to compile because its invalid wkt
        // wkt! { MULTIPOLYGON() }
    }

    #[test]
    fn multi_line_polygon() {
        let multi_polygon = wkt! { MULTIPOLYGON (((1.0 2.0))) };
        assert_eq!(multi_polygon.0.len(), 1);
        assert_eq!(multi_polygon.0[0].exterior().0[0], coord! { x: 1.0, y: 2.0});

        let multi_polygon = wkt! { MULTIPOLYGON (((1.0 2.0,3.0 4.0), (1.1 2.1,3.1 4.1), (1.2 2.2,3.2 4.2)),((1.0 2.0))) };
        assert_eq!(multi_polygon.0.len(), 2);
        assert_eq!(
            multi_polygon.0[0].interiors()[1].0[0],
            coord! { x: 1.2, y: 2.2}
        );

        let multi_polygon = wkt! { MULTIPOLYGON (((1.0 2.0,3.0 4.0), (1.1 2.1,3.1 4.1), (1.2 2.2,3.2 4.2)), EMPTY) };
        assert_eq!(multi_polygon.0.len(), 2);
        assert_eq!(
            multi_polygon.0[0].interiors()[1].0[0],
            coord! { x: 1.2, y: 2.2}
        );
        assert!(multi_polygon.0[1].exterior().0.is_empty());
    }

    #[test]
    fn empty_geometry_collection() {
        let geometry_collection: GeometryCollection = wkt! { GEOMETRYCOLLECTION EMPTY };
        assert!(geometry_collection.is_empty());

        // This (rightfully) fails to compile because its invalid wkt
        // wkt! { MULTIPOLYGON() }
    }

    #[test]
    fn geometry_collection() {
        let geometry_collection = wkt! {
            GEOMETRYCOLLECTION (
                POINT (40.0 10.0),
                LINESTRING (10.0 10.0, 20.0 20.0, 10.0 40.0),
                POLYGON ((40.0 40.0, 20.0 45.0, 45.0 30.0, 40.0 40.0))
            )
        };
        assert_eq!(geometry_collection.len(), 3);

        let line_string = match &geometry_collection[1] {
            Geometry::LineString(line_string) => line_string,
            _ => panic!(
                "unexpected geometry: {geometry:?}",
                geometry = geometry_collection[1]
            ),
        };
        assert_eq!(line_string.0[1], coord! {x: 20.0, y: 20.0 });
    }

    #[test]
    fn other_numeric_types() {
        let point: Point<i32> = wkt!(POINT(1 2));
        assert_eq!(point.x(), 1i32);
        assert_eq!(point.y(), 2i32);

        let point: Point<u64> = wkt!(POINT(1 2));
        assert_eq!(point.x(), 1u64);
        assert_eq!(point.y(), 2u64);

        let point: Point<f32> = wkt!(POINT(1.0 2.0));
        assert_eq!(point.x(), 1.0f32);
        assert_eq!(point.y(), 2.0f32);
    }
}
