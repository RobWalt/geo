use super::{
    node_map::{NodeFactory, NodeMap},
    CoordNode, CoordPos, Edge, Label,
};
use crate::{Coord, GeoFloat};

use std::cell::RefCell;
use std::rc::Rc;

pub(crate) struct PlanarGraphNode;

/// The basic node constructor does not allow for incident edges
impl<F> NodeFactory<F> for PlanarGraphNode
where
    F: GeoFloat,
{
    type Node = CoordNode<F>;
    fn create_node(coordinate: Coord<F>) -> Self::Node {
        CoordNode::new(coordinate)
    }
}

pub(crate) struct PlanarGraph<F: GeoFloat> {
    pub(crate) nodes: NodeMap<F, PlanarGraphNode>,
    edges: Vec<Rc<RefCell<Edge<F>>>>,
}

impl<F: GeoFloat> PlanarGraph<F> {
    pub fn edges(&self) -> &[Rc<RefCell<Edge<F>>>] {
        &self.edges
    }

    pub fn new() -> Self {
        PlanarGraph {
            nodes: NodeMap::new(),
            edges: vec![],
        }
    }

    pub fn is_boundary_node(&self, geom_index: usize, coord: Coord<F>) -> bool {
        self.nodes
            .find(coord)
            .and_then(|node| node.label().on_position(geom_index))
            .map(|position| position == CoordPos::OnBoundary)
            .unwrap_or(false)
    }

    pub fn insert_edge(&mut self, edge: Edge<F>) {
        self.edges.push(Rc::new(RefCell::new(edge)));
    }

    pub fn add_node_with_coordinate(&mut self, coord: Coord<F>) -> &mut CoordNode<F> {
        self.nodes.insert_node_with_coordinate(coord)
    }

    pub fn boundary_nodes(&self, geom_index: usize) -> impl Iterator<Item = &CoordNode<F>> {
        self.nodes.iter().filter(move |node| {
            matches!(
                node.label().on_position(geom_index),
                Some(CoordPos::OnBoundary)
            )
        })
    }
}
