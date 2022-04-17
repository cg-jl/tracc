use crate::intermediate::{analysis, Binding, BlockBinding, BlockEnd, Branch, Statement, IR};
use std::collections::{HashMap, HashSet};

pub type LifetimeMap = HashMap<Binding, Lifetime>;

// BUG: detects wrong collisions from phi nodes/branches:
/*
BB0:
  %0 = 2
  %1 = cmp eq, %0, 1
  br-cond %1, BB2, BB1
BB1:
  %3 = 3
  store %0, u32 %3
  br  BB2
BB2:
  %6 = phi [ %1, BB0 ], [ %3, BB1 ]
  ret %6

given this code, %1 does *NOT* collide with %3 because the path selected by the phi node
doesn't contain %3's lifetime.
  */

pub fn compute_lifetimes(ir: &IR) -> LifetimeMap {
    let mut lifetime_map = LifetimeMap::new();

    let mut die_map = get_lifetime_ends(ir);

    for (key, def) in get_defs(ir) {
        let die = die_map
            .remove(&key)
            .expect("every variable defined must die");
        debug_assert!(
            lifetime_map
                .insert(
                    key,
                    Lifetime {
                        attached_binding: key,
                        start: def,
                        ends: die
                            .into_iter()
                            .map(|blockaddr| (blockaddr.block, blockaddr.statement))
                            .collect(),
                    }
                )
                .is_none(),
            "each variable is declared once and dies once"
        );
    }

    lifetime_map
}

pub type CollisionMap = HashMap<Binding, HashSet<Binding>>;

pub fn compute_lifetime_collisions(ir: &IR) -> CollisionMap {
    let lifetime_map = compute_lifetimes(ir);
    lifetime_map
        .iter()
        .map(|(k, lifetime)| {
            (
                *k,
                lifetime_map
                    .iter()
                    .filter_map(|(k2, l2)| {
                        if k2 != k && lifetime.intersects(l2, ir) {
                            Some(*k2)
                        } else {
                            None
                        }
                    })
                    .collect(),
            )
        })
        .collect()
}

pub fn get_defs(ir: &IR) -> impl Iterator<Item = (Binding, BlockAddress)> + '_ {
    // go through each block and the statements which define a binding
    super::iterate_with_bindings(&ir.code).flat_map(|(block_binding, block)| {
        block
            .statements
            .iter()
            .enumerate()
            .filter_map(move |(statement_index, statement)| {
                if let Statement::Assign { index, .. } = statement {
                    Some((
                        *index,
                        BlockAddress {
                            block: block_binding,
                            statement: statement_index,
                        },
                    ))
                } else {
                    None
                }
            })
    })
}

fn get_lifetime_ends(ir: &IR) -> HashMap<Binding, Vec<BlockAddress>> {
    // #1. Get all block ends for each binding
    let all_block_ends = {
        let mut map: HashMap<Binding, HashMap<BlockBinding, usize>> = HashMap::new();
        for (block_binding, block) in analysis::iterate_with_bindings(&ir.code) {
            for (statement_index, statement) in block.statements.iter().enumerate() {
                use analysis::BindingUsage;
                for dep in statement.binding_deps() {
                    map.entry(dep)
                        .or_default()
                        .insert(block_binding, statement_index);
                }
            }
            match &block.end {
                BlockEnd::Branch(Branch::Conditional { flag, .. }) => {
                    map.entry(*flag)
                        .or_default()
                        .insert(block_binding, block.statements.len());
                }
                BlockEnd::Return(ret) => {
                    map.entry(*ret)
                        .or_default()
                        .insert(block_binding, block.statements.len());
                }
                _ => (),
            }
        }
        map
    };
    // #2. For each binding, remove all of the bindings that have their children in the same map
    all_block_ends
        .into_iter()
        .map(|(binding, mut die_map)| {
            let to_remove: Vec<_> = die_map
                .keys()
                .copied()
                .filter(|&k| {
                    analysis::predecessors(ir, k)
                        .skip(1)
                        .any(|child| die_map.contains_key(&child))
                })
                .collect();
            for k in to_remove {
                die_map.remove(&k);
            }
            (
                binding,
                die_map
                    .into_iter()
                    .map(|(block, statement)| BlockAddress { block, statement })
                    .collect(),
            )
        })
        .collect()
}
impl Lifetime {
    /// If the binding is local to the block, it will return the statement index
    pub fn local_end(&self) -> Option<(BlockBinding, usize)> {
        self.ends
            .get(&self.start.block)
            .map(|&index| (self.start.block, index))
    }
    fn start_from_block(&self, block: BlockBinding) -> Option<usize> {
        if block == self.start.block {
            Some(self.start.statement)
        } else {
            None
        }
    }
    /// Checks whether the binding is local to the block (is not used in other blocks)
    pub fn is_local_to_block(&self) -> bool {
        // if the code is correct, and the binding is local, once it has been ended in the block
        // it's defined then no more uses can happen
        self.ends.contains_key(&self.start.block)
    }
    pub fn intersects(&self, other: &Self, ir: &IR) -> bool {
        let self_passes_through: HashSet<_> = self.pass_through_list(ir).collect();
        let other_passes_through: HashSet<_> = other.pass_through_list(ir).collect();

        // they won't intersect if:
        //  - the blocks they pass through never coincide.
        self_passes_through
            .intersection(&other_passes_through)
            //  - if there is any block where they coincide:
            .any(|block| {
                //      - if one dies and the other one is defined in the same block:
                let dies_or_defined =
                    self.ends
                        .get(block)
                        .copied()
                        .and_then(|end| other.start_from_block(*block).map(|start| (end, start)))
                        .or_else(|| {
                            other.ends.get(block).copied().and_then(|end| {
                                self.start_from_block(*block).map(|start| (end, start))
                            })
                        });

                if let Some((dies, defined)) = dies_or_defined {
                    //          - if the one that dies has a higher statement index than the statement index
                    //              of the one that is defined, then they intersect.
                    dies > defined

                    //          - otherwise, they won't intersect (since one is defined after the other is
                    //          already dead)
                } else {
                    //      - otherwise, there is at least one that passes through the whole block, fully
                    //      intersecting the other one.
                    true
                }
            })
    }

    pub fn pass_through_list<'ir>(
        &'ir self,
        ir: &'ir IR,
    ) -> impl Iterator<Item = BlockBinding> + 'ir {
        super::predecessors(ir, self.start.block).filter(|block| {
            use super::BindingUsage;
            ir[*block].contains_binding(self.attached_binding)
        })
    }
}
#[derive(Clone)]
pub struct Lifetime {
    pub attached_binding: Binding,
    pub start: BlockAddress,
    pub ends: HashMap<BlockBinding, usize>,
}
impl std::fmt::Debug for Lifetime {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        struct AddressMapDebugImpl<'a>(&'a HashMap<BlockBinding, usize>);
        impl<'a> std::fmt::Debug for AddressMapDebugImpl<'a> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_list()
                    .entries(
                        self.0
                            .iter()
                            .map(|(&block, &statement)| BlockAddress { block, statement }),
                    )
                    .finish()
            }
        }
        f.debug_struct("Lifetime")
            .field("start", &self.start)
            .field("ends", &AddressMapDebugImpl(&self.ends))
            .finish()
    }
}
impl std::fmt::Debug for BlockAddress {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}[{}]", self.block, self.statement)
    }
}

// TODO: move `Debug` impls to a separate module
#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct BlockAddress {
    pub block: BlockBinding,
    pub statement: usize,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::intermediate::*;
    // TODO: more tests on intersections:
    //  - different blocks, collides
    //  - different blocks, different branches
    //  - same branch, one encloses the other
    //  - same branch, a is dead while b is alive, but b is defined when
    //  a is still alive.
    #[test]
    fn same_block_noncolliding() {
        let ir = IR::from(vec![BasicBlock {
            statements: vec![
                Statement::Assign {
                    index: Binding(0),
                    value: Value::Allocate { size: 4 },
                },
                Statement::Assign {
                    index: Binding(1),
                    value: Value::Constant(3),
                },
                Statement::Store {
                    binding: Binding(1),
                    mem_binding: Binding(0),
                    byte_size: ByteSize::U32,
                },
                Statement::Assign {
                    index: Binding(2),
                    value: Value::Allocate { size: 4 },
                },
                Statement::Store {
                    binding: Binding(1),
                    mem_binding: Binding(2),
                    byte_size: ByteSize::U32,
                },
            ],
            end: BlockEnd::Return(Binding(1)),
        }]);
        let lifetime_1 = Lifetime {
            attached_binding: Binding(0),
            start: BlockAddress {
                block: BlockBinding(0),
                statement: 0,
            },
            ends: vec![(BlockBinding(0), 2)].into_iter().collect(),
        };
        let lifetime_2 = Lifetime {
            attached_binding: Binding(2),
            start: BlockAddress {
                block: BlockBinding(0),
                statement: 3,
            },
            ends: vec![(BlockBinding(0), 4)].into_iter().collect(),
        };
        assert!(!lifetime_1.intersects(&lifetime_2, &ir));
    }

    #[test]
    fn only_block_passes_through() {
        let ir = IR::from(vec![BasicBlock {
            statements: vec![Statement::Assign {
                index: Binding(0),
                value: Value::Constant(0),
            }],
            end: BlockEnd::Return(Binding(0)),
        }]);
        let lifetime_map = compute_lifetimes(&ir);
        dbg!(&lifetime_map);
        let lifetime = &lifetime_map[&Binding(0)];
        dbg!(&lifetime);
        assert_eq!(
            lifetime.pass_through_list(&ir).collect::<Vec<_>>(),
            vec![BlockBinding(0)]
        )
    }

    /// sets up a triangle CFG:
    ///         BB1
    ///       /
    ///    BB0
    ///       \
    ///         BB2
    /// and sets up two bindings in BB1 and BB2 as well.
    /// each binding dies in its own block, so there should be no intersections.
    #[test]
    fn no_blocks_in_common() {
        let ir = IR::from(vec![
            BasicBlock {
                statements: vec![Statement::Assign {
                    index: Binding(0),
                    value: Value::Constant(0),
                }],
                end: BlockEnd::Branch(Branch::Conditional {
                    flag: Binding(0),
                    target_true: BlockBinding(1),
                    target_false: BlockBinding(2),
                }),
            },
            BasicBlock {
                statements: vec![Statement::Assign {
                    index: Binding(1),
                    value: Value::Constant(1),
                }],
                end: BlockEnd::Return(Binding(1)),
            },
            BasicBlock {
                statements: vec![Statement::Assign {
                    index: Binding(2),
                    value: Value::Constant(0),
                }],
                end: BlockEnd::Return(Binding(2)),
            },
        ]);
        dbg!(&ir);
        let lifetimes = compute_lifetimes(&ir);
        dbg!(&lifetimes);
        let lifetimes: Vec<_> = lifetimes.into_values().collect();

        assert!(!lifetimes[0].intersects(&lifetimes[1], &ir));
        assert!(!lifetimes[0].intersects(&lifetimes[2], &ir));

        assert!(!lifetimes[1].intersects(&lifetimes[2], &ir));
        assert!(!lifetimes[1].intersects(&lifetimes[0], &ir));

        assert!(!lifetimes[2].intersects(&lifetimes[1], &ir));
        assert!(!lifetimes[2].intersects(&lifetimes[0], &ir));
    }

    #[test]
    fn correct_branch_pass_through_list() {
        const MESSAGE: &str = r#"
given this code, %0 does *NOT* collide with %1 because the path selected by the phi node
doesn't contain %1's lifetime.
        "#;
        let ir = IR::from(vec![
            BasicBlock {
                statements: vec![Statement::Assign {
                    index: Binding(0),
                    value: Value::Constant(2),
                }],
                end: BlockEnd::Branch(Branch::Conditional {
                    flag: Binding(0),
                    target_true: BlockBinding(1),
                    target_false: BlockBinding(2),
                }),
            },
            BasicBlock {
                statements: vec![Statement::Assign {
                    index: Binding(3),
                    value: Value::Constant(3),
                }],
                end: BlockEnd::Branch(Branch::Unconditional {
                    target: BlockBinding(1),
                }),
            },
            BasicBlock {
                statements: vec![Statement::Assign {
                    index: Binding(2),
                    value: Value::Phi {
                        nodes: vec![
                            PhiDescriptor {
                                value: Binding(0),
                                block_from: BlockBinding(0),
                            },
                            PhiDescriptor {
                                value: Binding(1),
                                block_from: BlockBinding(1),
                            },
                        ],
                    },
                }],
                end: BlockEnd::Return(Binding(2)),
            },
        ]);

        let lifetimes = compute_lifetimes(&ir);
        assert!(
            !lifetimes[&Binding(0)].intersects(&lifetimes[&Binding(1)], &ir),
            "{:?}\n{}",
            ir,
            MESSAGE,
        );
    }
}
