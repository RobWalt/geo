use crate::algorithm::spade_boolops::trait_def::SpadeBoolops;
use std::fmt::Display;
use std::str::FromStr;

use geo_types::*;
use wkt::TryFromWkt;

use crate::GeoFloat;

fn check_op<T: GeoFloat + FromStr + Default + Display>(
    wkt1: &str,
    wkt2: &str,
) -> [MultiPolygon<T>; 2] {
    _ = pretty_env_logger::try_init();
    let poly1 = MultiPolygon::<T>::try_from_wkt_str(wkt1)
        .or_else(|_| Polygon::<T>::try_from_wkt_str(wkt1).map(MultiPolygon::from))
        .unwrap();
    let poly2 = MultiPolygon::try_from_wkt_str(wkt2)
        .or_else(|_| Polygon::<T>::try_from_wkt_str(wkt2).map(MultiPolygon::from))
        .unwrap();

    [poly1, poly2]
}

#[test]
fn test_rect_overlapping() {
    // Two rects that overlap
    let wkt1 = "POLYGON((0 0,1 0,1 1,0 1,0 0))";
    let wkt2 = "POLYGON((0.5 1,2 1,2 2,0.5 2,0.5 1))";

    let wkt_union = "MULTIPOLYGON(((0.5 2,0.5 1,0 1,0 0,1 0,1 1,2 1,2 2,0.5 2)))";
    let [p1, p2] = check_op::<f64>(wkt1, wkt2);
    let output = MultiPolygon::union(&p1, &p2).expect("boolop works");
    assert_eq!(output, MultiPolygon::try_from_wkt_str(wkt_union).unwrap());
}

#[test]
fn test_ext_in_hole() {
    // A union which outputs a ring inside a hole inside a ext.
    let wkt1 = "POLYGON((0 0, 40 0, 40 40, 0 40, 0 0), (10 10, 30 10, 30 30, 10 30, 10 10))";
    let wkt2 = "POLYGON((11 11, 29 11, 29 29, 11 29, 11 11), (15 15, 25 15, 25 25, 15 25, 15 15))";
    let [p1, p2] = check_op::<f64>(wkt1, wkt2);
    let _valid_output = MultiPolygon::union(&p1, &p2).expect("boolop works");
}

#[test]
fn test_invalid_simple() {
    // Polygon with holes and invalid
    let wkt1 = "POLYGON((0 0, 2 2, 2 0, 0 0), (1 1, 2 1, 1 0))";
    let wkt2 = "POLYGON EMPTY";
    let [p1, p2] = check_op::<f64>(wkt1, wkt2);
    let _valid_output = MultiPolygon::union(&p1, &p2).expect("boolop works");
}

#[test]
fn test_invalid_loops() {
    let wkt1 = "POLYGON((0 0, 2 2, 0 4, -2 2, 0 0, 1 2, 0 3, -1 2, 0 0))";
    let wkt2 = "POLYGON EMPTY";
    let [p1, p2] = check_op::<f64>(wkt1, wkt2);
    let _valid_output = MultiPolygon::union(&p1, &p2).expect("boolop works");
}

#[test]
fn test_complex_rects() {
    let wkt1 = "MULTIPOLYGON(((-1 -2,-1.0000000000000002 2,-0.8823529411764707 2,-0.8823529411764706 -2,-1 -2)),((-0.7647058823529411 -2,-0.7647058823529412 2,-0.6470588235294118 2,-0.6470588235294118 -2,-0.7647058823529411 -2)),((-0.5294117647058824 -2,-0.5294117647058825 2,-0.41176470588235287 2,-0.4117647058823529 -2,-0.5294117647058824 -2)),((-0.2941176470588236 -2,-0.2941176470588236 2,-0.17647058823529418 2,-0.17647058823529416 -2,-0.2941176470588236 -2)),((-0.05882352941176472 -2,-0.05882352941176472 2,0.05882352941176472 2,0.05882352941176472 -2,-0.05882352941176472 -2)),((0.17647058823529416 -2,0.17647058823529416 2,0.29411764705882365 2,0.2941176470588236 -2,0.17647058823529416 -2)),((0.4117647058823528 -2,0.41176470588235287 2,0.5294117647058821 2,0.5294117647058822 -2,0.4117647058823528 -2)),((0.6470588235294117 -2,0.6470588235294118 2,0.7647058823529411 2,0.7647058823529411 -2,0.6470588235294117 -2)),((0.8823529411764706 -2,0.8823529411764707 2,1.0000000000000002 2,1 -2,0.8823529411764706 -2)))";
    let wkt2 = "MULTIPOLYGON(((-2 -1,2 -1.0000000000000002,2 -0.8823529411764707,-2 -0.8823529411764706,-2 -1)),((-2 -0.7647058823529411,2 -0.7647058823529412,2 -0.6470588235294118,-2 -0.6470588235294118,-2 -0.7647058823529411)),((-2 -0.5294117647058824,2 -0.5294117647058825,2 -0.41176470588235287,-2 -0.4117647058823529,-2 -0.5294117647058824)),((-2 -0.2941176470588236,2 -0.2941176470588236,2 -0.17647058823529418,-2 -0.17647058823529416,-2 -0.2941176470588236)),((-2 -0.05882352941176472,2 -0.05882352941176472,2 0.05882352941176472,-2 0.05882352941176472,-2 -0.05882352941176472)),((-2 0.17647058823529416,2 0.17647058823529416,2 0.29411764705882365,-2 0.2941176470588236,-2 0.17647058823529416)),((-2 0.4117647058823528,2 0.41176470588235287,2 0.5294117647058821,-2 0.5294117647058822,-2 0.4117647058823528)),((-2 0.6470588235294117,2 0.6470588235294118,2 0.7647058823529411,-2 0.7647058823529411,-2 0.6470588235294117)),((-2 0.8823529411764706,2 0.8823529411764707,2 1.0000000000000002,-2 1,-2 0.8823529411764706)))";

    let mp1 = MultiPolygon::<f64>::try_from_wkt_str(wkt1).unwrap();
    let mp2 = MultiPolygon::<f64>::try_from_wkt_str(wkt2).unwrap();

    for p1 in mp1.0.iter() {
        for p2 in mp2.0.iter() {
            let _valid = Polygon::union(p1, p2);
        }
    }
}

#[test]
fn test_complex_rects1() {
    let wkt1 = "MULTIPOLYGON(((-1 -2,-1.0000000000000002 2,-0.8823529411764707 2,-0.8823529411764706 -2,-1 -2)))";
    let wkt2 = "MULTIPOLYGON(((-2 -1,2 -1.0000000000000002,2 -0.8823529411764707,-2 -0.8823529411764706,-2 -1)))";
    let [p1, p2] = check_op::<f64>(wkt1, wkt2);
    let _valid_output = MultiPolygon::union(&p1, &p2).expect("boolop works");
}

#[test]
fn test_overlap_issue_867() {
    let wkt1 = "POLYGON ((17.724912058920285 -16.37118892052372, 18.06452454246989 -17.693907532504, 19.09389292605319 -17.924001641855178, 17.724912058920285 -16.37118892052372))";
    let wkt2 = "POLYGON ((17.576085274796423 -15.791540153598898, 17.19432983818328 -17.499393422066746, 18.06452454246989 -17.693907532504, 17.576085274796423 -15.791540153598898))";
    let [p1, p2] = check_op::<f64>(wkt1, wkt2);
    let _valid_output = MultiPolygon::intersection(&p1, &p2).expect("boolop works");
}

#[test]
fn test_issue_865() {
    // Simplified example
    let wkt1 = "POLYGON((0 0, 1 1, 10 0, 0 0))";
    let wkt2 = "POLYGON((1 1, 8 2, 4 1, 1 1))";
    let wkt2d = "POLYGON((1 1, 4 2, 8 1, 1 1))";

    let [p1, p2] = check_op::<f64>(wkt1, wkt2);
    let _valid_output = MultiPolygon::union(&p1, &p2).expect("boolop works");

    let [p1, p2] = check_op::<f64>(wkt1, wkt2d);
    let _valid_output = MultiPolygon::union(&p1, &p2).expect("boolop works");

    // Original Example
    let wkt1 = "POLYGON((-640 -360,640 -360,640 360,-640 360,-640 -360))";
    let wkt2 = "POLYGON((313.276 359.999,213.319 359.999,50 60,-50 60,-50 110,-8.817 360,-93.151 360,-85.597 225.618,-114.48 359.999,-117.017 360,-85 215,-85 155,-115 155,-154.161 360,-640 360,-640 -360,640 -360,640 360,313.277 360,313.276 359.999))";

    let [p1, p2] = check_op::<f64>(wkt1, wkt2);
    let _valid_output = MultiPolygon::difference(&p1, &p2).expect("boolop works");
}

#[test]
fn test_issue_885_small() {
    let wkt1 = "POLYGON((8055.658 7977.5537,8010.734 7999.9697,8032.9717 8044.537,8077.896 8022.121,8055.658 7977.5537))";
    let wkt2 = "POLYGON((8055.805 7977.847,8010.871 8000.2676,8033.105 8044.8286,8078.039 8022.408,8055.805 7977.847))";

    let [p1, p2] = check_op::<f64>(wkt1, wkt2);
    let _valid_output = MultiPolygon::union(&p1, &p2).expect("boolop works");
}

#[test]
fn test_issue_885_big_raw() {
    let wkt1 = "MULTIPOLYGON(((256 -256,-256 -256,-256 256,256 256,256 -256),(21.427018453144548 100.2247496417564,22.170654296875 97.449462890625,21.255590787413905 95.86452640008609,22.170654296875 92.449462890625,21.255635468249327 90.86460378956318,22.170654296875 87.44970703125,21.255590787413905 85.86477054071109,22.170654296875 82.44970703125,21.255590787413905 80.86477054071109,22.170654296875 77.44970703125,21.255635468249327 75.86484793018818,22.170654296875 72.449951171875,21.255590787413905 70.86501468133609,22.170654296875 67.449951171875,21.255590787413905 65.86501468133609,22.170654296875 62.449951171875,21.255635468249327 60.865092070813176,22.170654296875 57.4501953125,20.964468977514716 55.3610210560243,21.7110595703125 52.57470703125,6.7110595703125036 26.593944917716843,4.94307055353958 26.120213688445443,3.1754150390625036 23.058544527091843,1.4077371841677144 22.584896673394404,-0.36010742187499645 19.522899995841843,-2.127952027917708 19.04920746130898,-3.8956298828124964 15.987499605216843,-5.663474488855209 15.513807070683983,-7.4311523437499964 12.452099214591843,-9.199141360522919 11.978367985320444,-10.966796874999996 8.916698823966843,-12.734474729894785 8.443050970269406,-14.502319335937496 5.381054292716843,-29.502319335937507 1.36181640625,-44.502319335937514 5.381054292716847,-55.48308144947066 16.361816406249996,-59.5023193359375 31.361816406250004,-55.48308144947066 46.36181640625,-51.94760366936858 49.89729418635208,-51.94755898853316 49.8974609375,-40.96679687499999 60.87822305103316,-40.96646337270415 60.878312412704005,-37.43115234374999 64.41362344165816,-37.43098559260209 64.41366812249358,-37.25638533366225 64.58826838143341,-37.15947272204719 64.949951171875,-37.829345703125 67.449951171875,-37.15947272204719 69.949951171875,-37.829345703125 72.449951171875,-37.159505430688846 74.9498291015625,-37.829345703125 77.44970703125,-37.15947272204719 79.94970703125,-37.829345703125 82.44970703125,-37.15947272204719 84.94970703125,-37.829345703125 87.44970703125,-37.6218793727004 88.22398191725448,-43.845336914062514 89.89155233959184,-54.82609902759566 100.872314453125,-58.8453369140625 115.872314453125,-54.82609902759566 130.872314453125,-43.84533691406249 141.85307656665816,-28.845336914062496 145.872314453125,-26.345275878906246 145.20242511772636,-23.845214843749996 145.872314453125,-21.345153808593746 145.20242511772636,-18.845092773437496 145.872314453125,-16.345092773437496 145.20244147204718,-13.845092773437498 145.872314453125,-11.345092773437498 145.20244147204718,-8.845092773437498 145.872314453125,-7.241160801189184 145.4425421764466,-4.753417968749998 146.109130859375,-3.857349940998309 145.86903015497558,-3.8450927734374982 145.872314453125,-2.5414695567288628 145.52300966497347,-0.04333496093749816 146.1923828125,14.956665039062504 142.17314492603316,14.96700942550945 142.16280053958621,15.549804687500004 142.00664101978316,26.53056680103316 131.02587890625,30.5498046875 116.02587890625,21.427018453144548 100.2247496417564),(15.9573974609375 218.888916015625,11.93815957447066 233.888916015625,0.9573974609375036 244.86967812915816,-14.042602539062498 248.888916015625,-29.042602539062493 244.86967812915816,-32.57795824885207 241.3343224193686,-32.57812499999999 241.33427773853316,-36.11348070978957 237.7989220287436,-36.11364746093749 237.79887734790816,-39.64895848989165 234.26356631895402,-39.64929199218749 234.26347695728316,-50.63005410572066 223.28271484375,-50.63009878655608 223.28254809260207,-54.16557656665816 219.7470703125,-58.184814453125 204.7470703125,-54.16557656665816 189.7470703125,-43.184814453125014 178.76630819896684,-28.184814453125007 174.7470703125,-13.184814453124996 178.76630819896684,-11.416969847082285 181.8283048765194,-9.649291992187496 182.30195273021684,-7.88163647771042 185.36362189157043,-6.1136474609374964 185.83735312084184,-4.345969606042708 188.89906097693398,-2.5781249999999964 189.37275351146684,-0.8104471451052081 192.43446136755898,0.9573974609375036 192.90815390209184,15.9573974609375 218.888916015625)))";
    let wkt2 = "POLYGON((19.492919921875 222.424560546875,15.47368203540816 237.424560546875,4.4929199218750036 248.40532266040816,-10.507080078124998 252.424560546875,-25.507080078124993 248.40532266040816,-36.48784219165816 237.424560546875,-40.507080078125 222.424560546875,-36.48784219165816 207.424560546875,-25.507080078125014 196.44379843334184,-10.507080078125005 192.424560546875,4.4929199218750036 196.44379843334184,19.492919921875 222.424560546875))";

    let [p1, p2] = check_op::<f64>(wkt1, wkt2);
    let _valid_output = MultiPolygon::difference(&p1, &p2).expect("boolop works");
}

#[test]
fn test_issue_885_big_simplified() {
    let wkt1 = r#"MULTIPOLYGON(((256.0 -256.0,-256.0 -256.0,-256.0 256.0,256.0 256.0,256.0 -256.0), (15.9573974609375 218.888916015625,-29.042602539062493 244.86967812915816, -32.57795824885207 241.3343224193686, -32.57812499999999 241.33427773853316, -36.11348070978957 237.7989220287436,-36.11364746093749 237.79887734790816, -39.64895848989165 234.26356631895402,0.9573974609375036 192.90815390209184,15.9573974609375 218.888916015625)))"#;
    let wkt2 = r#"POLYGON((19.492919921875 222.424560546875,-25.507080078124993 248.40532266040816,-36.48784219165816 237.424560546875, 4.4929199218750036 196.44379843334184,19.492919921875 222.424560546875))"#;

    let [p1, p2] = check_op::<f64>(wkt1, wkt2);
    let _valid_output = MultiPolygon::difference(&p1, &p2).expect("boolop works");
}

// care: This test highlights that the SpadeBoolops algo is awfully slow! :S
#[test]
fn test_issue_894() {
    _ = pretty_env_logger::try_init();
    use geo_test_fixtures::multi_polygon;
    let a: MultiPolygon<f64> = multi_polygon("issue-894/inpa.wkt");
    let b = multi_polygon("issue-894/inpb.wkt");
    let c = multi_polygon("issue-894/inpc.wkt");

    let aib = MultiPolygon::intersection(&a, &b).unwrap();
    log::info!("first");
    MultiPolygon::intersection(&b, &c).unwrap();
    log::info!("second");
    MultiPolygon::intersection(&aib, &c).unwrap();
    log::info!("third");
}

#[test]
fn test_issue_buffer_box() {
    // TODO: This test-case breaks the ordering assumption, and does not pass.
    let wkt1 = "MULTIPOLYGON(((-164.93595896333647 152.85701803397086,-164.93595896333647 149.53568721641966,-51.873865625542294 153.2197777241044,-51.873865625542294 255.14903655104297,-153.80312445248086 255.14903655104297,-266.86521779027504 251.46494604335822,-266.86521779027504 149.53568721641966,-164.93595896333647 152.85701803397086)))";
    let wkt2 = "MULTIPOLYGON(((-164.93595896333647 149.53568721641966,-51.873865625542294 153.2197777241044,-153.80312445248086 153.2197777241044,-266.86521779027504 149.53568721641966,-164.93595896333647 149.53568721641966)))";

    let [p1, p2] = check_op::<f64>(wkt1, wkt2);
    let _valid_output = MultiPolygon::union(&p1, &p2).expect("boolop works");
}
