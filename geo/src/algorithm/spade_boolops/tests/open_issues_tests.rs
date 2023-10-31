use crate::SpadeBoolops;
use geo_types::{Coord, LineString, MultiPolygon, Polygon};
use wkt::TryFromWkt;

#[test]
fn no_1103_union_for_f32_polys() {
    let polygons = [
        Polygon::<f32>::new(
            LineString::from(vec![
                Coord {
                    x: 3.349_365_2,
                    y: -55.801_27,
                },
                Coord {
                    x: 171.224_43,
                    y: -300.0,
                },
                Coord {
                    x: 291.841_64,
                    y: -300.0,
                },
                Coord {
                    x: 46.650_635,
                    y: -30.801_27,
                },
                Coord {
                    x: 3.349_365_2,
                    y: -55.801_27,
                },
            ]),
            Vec::new(),
        ),
        Polygon::<f32>::new(
            LineString::from(vec![
                Coord {
                    x: 46.650_635,
                    y: -30.801_27,
                },
                Coord {
                    x: 291.841_64,
                    y: -300.0,
                },
                Coord {
                    x: 300.0,
                    y: -228.340_03,
                },
                Coord {
                    x: -3.349_365_2,
                    y: 55.801_27,
                },
                Coord {
                    x: 46.650_635,
                    y: -30.801_27,
                },
            ]),
            Vec::new(),
        ),
        Polygon::<f32>::new(
            LineString::from(vec![
                Coord {
                    x: -46.650_635,
                    y: 30.801_27,
                },
                Coord {
                    x: 195.837_28,
                    y: -300.0,
                },
                Coord {
                    x: 300.0,
                    y: -228.340_03,
                },
                Coord {
                    x: -3.349_365_2,
                    y: 55.801_27,
                },
                Coord {
                    x: -46.650_635,
                    y: 30.801_27,
                },
            ]),
            Vec::new(),
        ),
        Polygon::<f32>::new(
            LineString::from(vec![
                Coord {
                    x: 3.349_365_2,
                    y: -55.801_27,
                },
                Coord {
                    x: 171.224_43,
                    y: -300.0,
                },
                Coord {
                    x: 195.837_28,
                    y: -300.0,
                },
                Coord {
                    x: -46.650_635,
                    y: 30.801_27,
                },
                Coord {
                    x: 3.349_365_2,
                    y: -55.801_27,
                },
            ]),
            Vec::new(),
        ),
    ];

    let mut multi = MultiPolygon::new(Vec::new());
    for poly in polygons {
        multi = MultiPolygon::union(&multi, &MultiPolygon::new(vec![poly])).unwrap();
    }
}

#[test]
fn no_1103_union_for_f64_polys() {
    let polygons = [
        Polygon::<f64>::new(
            LineString::from(vec![
                Coord {
                    x: 3.349365234375,
                    y: -55.80126953125,
                },
                Coord {
                    x: 171.224_426_269_531_25,
                    y: -300.0,
                },
                Coord {
                    x: 291.841_644_287_109_4,
                    y: -300.0,
                },
                Coord {
                    x: 46.650_634_765_625,
                    y: -30.801_269_531_25,
                },
                Coord {
                    x: 3.349_365_234_375,
                    y: -55.801_269_531_25,
                },
            ]),
            Vec::new(),
        ),
        Polygon::<f64>::new(
            LineString::from(vec![
                Coord {
                    x: 46.650_634_765_625,
                    y: -30.801_269_531_25,
                },
                Coord {
                    x: 291.841_644_287_109_4,
                    y: -300.0,
                },
                Coord {
                    x: 300.0,
                    y: -228.340_026_855_468_75,
                },
                Coord {
                    x: -3.349_365_234_375,
                    y: 55.801_269_531_25,
                },
                Coord {
                    x: 46.650_634_765_625,
                    y: -30.801_269_531_25,
                },
            ]),
            Vec::new(),
        ),
        Polygon::<f64>::new(
            LineString::from(vec![
                Coord {
                    x: -46.650_634_765_625,
                    y: 30.801_269_531_25,
                },
                Coord {
                    x: 195.837_280_273_437_5,
                    y: -300.0,
                },
                Coord {
                    x: 300.0,
                    y: -228.340_026_855_468_75,
                },
                Coord {
                    x: -3.349_365_234_375,
                    y: 55.801_269_531_25,
                },
                Coord {
                    x: -46.650_634_765_625,
                    y: 30.801_269_531_25,
                },
            ]),
            Vec::new(),
        ),
        Polygon::<f64>::new(
            LineString::from(vec![
                Coord {
                    x: 3.349_365_234_375,
                    y: -55.801_269_531_25,
                },
                Coord {
                    x: 171.224_426_269_531_25,
                    y: -300.0,
                },
                Coord {
                    x: 195.837_280_273_437_5,
                    y: -300.0,
                },
                Coord {
                    x: -46.650_634_765_625,
                    y: 30.801_269_531_25,
                },
                Coord {
                    x: 3.349_365_234_375,
                    y: -55.801_269_531_25,
                },
            ]),
            Vec::new(),
        ),
    ];

    let mut multi = MultiPolygon::new(Vec::new());
    for poly in polygons {
        multi = MultiPolygon::union(&multi, &MultiPolygon::new(vec![poly])).unwrap();
    }
}

#[test]
fn no_1053_intersection_for_f32_polys() {
    // Reproduction occurs when intersecting Polygon<f32> types, but Polygon<f64> does not reproduce.
    let geo1 = Polygon::<f32>::new(
        LineString(vec![
            Coord { x: 0.0, y: 0.0 },
            Coord { x: 0.0, y: 200.0 },
            Coord { x: 200.0, y: 200.0 },
            Coord { x: 200.0, y: 0.0 },
            Coord { x: 0.0, y: 0.0 },
        ]),
        vec![],
    );
    let geo2 = Polygon::<f32>::new(
        LineString(vec![
            Coord {
                x: -0.17588139,
                y: 0.0015348792,
            },
            Coord {
                x: 1.5845897,
                y: 201.73154,
            },
            Coord {
                x: 200.1759,
                y: 199.99846,
            },
            Coord {
                x: 198.41544,
                y: -1.7315454,
            },
            Coord {
                x: -0.17588139,
                y: 0.0015348792,
            },
        ]),
        vec![],
    );
    let _valid_result = Polygon::intersection(&geo1, &geo2).unwrap();
}

#[test]
fn no_1064_intersection_for_f64() {
    let a: Polygon = Polygon::try_from_wkt_str("POLYGON ((709799.9999999999 4535330.115932672, 709800.0000000001 4535889.8772568945, 709800.0057476197 4535889.994252375, 709800.1227431173 4535890.000000001, 710109.8788852151 4535889.999999996, 710109.994510683 4535889.99439513, 710110.0025226776 4535889.878911494, 710119.9974094903 4535410.124344491, 710119.9939843 4535410.005891683, 710119.8756285358 4535410.000000003, 710050.1240139506 4535410.000000003, 710050.0040320279 4535410.005955433, 710049.9539423736 4535410.115144073, 710038.1683017325 4535439.579245671, 710037.9601922985 4535440.0325333765, 710037.7079566419 4535440.462831489, 710037.4141048047 4535440.865858022, 710037.0815609565 4535441.237602393, 710036.7136342974 4535441.57436531, 710036.3139861295 4535441.872795589, 710035.8865934211 4535442.129923492, 710035.4357092284 4535442.343190307, 710034.9658203793 4535442.510473766, 710034.4816028172 4535442.6301092245, 710033.9878750739 4535442.7009061435, 710033.489550319 4535442.722160025, 710032.9915874689 4535442.6936593605, 710032.4989418354 4535442.615687774, 710032.016515821 4535442.48902117, 710031.54911013 4535442.314920021, 710031.1013759954 4535442.095116852, 710030.6777688961 4535441.831798956, 710030.2825042206 4535441.527586658, 710029.9195153153 4535441.185507218, 710029.5924143443 4535440.808964732, 710029.3044563448 4535440.401706239, 710029.0585068383 4535439.967784446, 710028.8570133082 4535439.511517367, 710028.701980853 4535439.037445406, 710028.5949522265 4535438.550286128, 710028.536992489 4535438.05488734, 710020.008429941 4535310.126449097, 710019.9938185212 4535310.0024022255, 710019.9208325277 4535309.901040657, 709980.0772727348 4535260.096590921, 709979.9987806884 4535260.007853539, 709979.8970781134 4535260.068614591, 709920.1022372885 4535299.931841814, 709920.0058878824 4535300.003151096, 709920.0000000001 4535300.122873942, 709920.0000000002 4535324.451138855, 709919.9762868701 4535324.937522437, 709919.9053724057 4535325.419292543, 709919.7879292492 4535325.891879465, 709919.6250713767 4535326.350800604, 709919.4183435373 4535326.7917029755, 709919.1697065973 4535327.210404501, 709918.8815189389 4535327.602933702, 709918.5565140966 4535327.965567328, 709918.1977748218 4535328.294865718, 709917.8087038476 4535328.5877053905, 709917.3929916087 4535328.841308688, 709916.9545812428 4535329.053270123, 709916.497631181 4535329.221579175, 709916.0264757108 4535329.344639407, 709915.5455838591 4535329.421283546, 709915.059517008 4535329.450784619, 709914.5728856224 4535329.43286279, 709914.0903055237 4535329.367688051, 709913.6163541072 4535329.255878603, 709913.1555269207 4535329.098494996, 709912.7121950255 4535328.89703004, 709912.2905635366 4535328.653394689, 709911.8946317348 4535328.369899881, 709911.5281551343 4535328.049234638, 709911.1946098561 4535327.694440548, 709910.8971596619 4535327.308882924, 709910.638625942 4535326.896218885, 709910.4214609532 4535326.460362643, 709910.2477245614 4535326.005448406, 709910.119064699 4535325.53579115, 709900.0266965557 4535280.120134492, 709899.9954816136 4535280.006411615, 709899.8778852832 4535280.015264336, 709820.1219250998 4535289.984759362, 709820.0038570677 4535290.005451573, 709819.9450491015 4535290.109901799, 709800.0518466672 4535329.896306664, 709800.0053207672 4535330.001256061, 709799.9999999999 4535330.115932672))").unwrap();
    let b: Polygon = Polygon::try_from_wkt_str("POLYGON ((709800 4600020, 809759.9999999993 4600020, 809759.9999999987 4500000, 709800.0000000003 4500000, 709800 4590240, 709800 4600020))").unwrap();

    Polygon::intersection(&a, &b).unwrap();
}
