use crate::intermediate::{
    analysis, Binding, BlockBinding, BlockEnd, Branch, Statement, Value, IR,
};
use std::{
    collections::{HashMap, HashSet},
    ops::ControlFlow,
};

pub type LifetimeMap = HashMap<Binding, Lifetime>;

pub fn compute_lifetimes(ir: &IR) -> Vec<Lifetime> {
    let mut lifetime_map = LifetimeMap::new();

    let mut die_map = get_lifetime_ends(ir);

    for (key, def) in get_defs(ir) {
        let die = match die_map.remove(&key) {
            Some(die) => die,
            // if a defined value wasn't caught on another statement, then it wasn't used.
            None => {
                lifetime_map.insert(
                    key,
                    Lifetime {
                        attached_binding: key,
                        start: def,
                        ends: vec![def],
                    },
                );
                continue;
            }
        };
        debug_assert!(
            lifetime_map
                .insert(
                    key,
                    Lifetime {
                        attached_binding: key,
                        start: def,
                        ends: if die.is_empty() { vec![def] } else { die }
                    }
                )
                .is_none(),
            "each variable is declared once and dies once"
        );
    }

    lifetime_map.into_values().collect()
}

pub type CollisionMap = HashMap<Binding, HashSet<Binding>>;
pub type CollisionMapWithLocations =
    HashMap<Binding, HashMap<Binding, (BlockAddress, BlockAddress)>>;

pub fn compute_lifetime_collisions(ir: &IR, lifetimes: &[Lifetime]) -> CollisionMap {
    let with_locations = compute_lifetime_collisions_with_locations(ir, lifetimes);
    use crate::intermediate::Value;

    with_locations
        .into_iter()
        .map(|(k, collisions)| {
            (
                k,
                collisions
                    .into_iter()
                    .filter_map(|(collision, mut locations)| {
                        locations.retain(|(_, end)| {
                            // filter out the locations that include a phi node with those two values.
                            let nodes = if let Some(Statement::Assign {
                                index: _,
                                value: Value::Phi { nodes },
                            }) = ir.get_statement(end)
                            {
                                nodes
                            } else {
                                return true;
                            };
                            let found_k = nodes.iter().find(|desc| desc.value == k).is_some();
                            let found_col =
                                nodes.iter().find(|desc| desc.value == collision).is_some();
                            !(found_k && found_col)
                        });

                        if locations.is_empty() {
                            None
                        } else {
                            Some(collision)
                        }
                    })
                    .collect(),
            )
        })
        .collect()
}

fn compute_lifetime_collisions_with_locations<'code>(
    ir: &'code IR,
    lifetimes: &'code [Lifetime],
) -> impl Iterator<
    Item = (
        Binding,
        HashMap<Binding, HashSet<(BlockAddress, BlockAddress)>>,
    ),
> + 'code {
    lifetimes.iter().map(|lifetime| {
        let mut locations = HashMap::new();
        lifetimes
            .iter()
            .filter(|l| l.attached_binding != lifetime.attached_binding)
            .for_each(|l| {
                let intersections = lifetime.find_intersections(l, ir);
                if !intersections.is_empty() {
                    locations.insert(l.attached_binding, intersections);
                }
            });
        (lifetime.attached_binding, locations)
    })
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
                statement.visit_value_bindings(&mut |dep| {
                    map.entry(dep)
                        .or_default()
                        .insert(block_binding, statement_index);
                    std::ops::ControlFlow::<(), _>::Continue(())
                });
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
    // pub fn local_end(&self) -> Option<BlockAddress> {
    //     self.endjjj
    //     self.ends
    //         .get(&self.start.block)
    //         .map(|&index| (self.start.block, index))
    // }
    // fn start_from_block(&self, block: BlockBinding) -> Option<usize> {
    //     if block == self.start.block {
    //         Some(self.start.statement)
    //     } else {
    //         None
    //     }
    // }
    /// Checks whether the binding is local to the block (is not used in other blocks)
    // pub fn is_local_to_block(&self) -> bool {
    //     // if the code is correct, and the binding is local, once it has been ended in the block
    //     // it's defined then no more uses can happen
    //     self.ends.contains_key(&self.start.block)
    // }
    pub fn find_intersections(
        &self,
        other: &Self,
        ir: &IR,
    ) -> HashSet<(BlockAddress, BlockAddress)> {
        // Lifetime A intersects lifetime B if:
        //   - for any end A' of A, B is comprised between A and A'
        //   OR (viceversa)
        //   - for any end B' of B, A is comprised between B and B'
        //   OR
        //   - A is defined before B and A has no end to it
        //   OR
        //   - B is defined before A and B has no end to it
        assert!(
            !self.ends.is_empty() && !other.ends.is_empty(),
            "({a} | {b}) everything comes to an end :(",
            a = self.attached_binding,
            b = other.attached_binding
        );

        if self.start.happens_before(ir, other.start) {
            self.ends
                .iter()
                .filter(|&a_end| other.start.happens_before(ir, *a_end))
                .filter(|&a_end| {
                    other
                        .ends
                        .iter()
                        .any(|b_end| a_end.happens_before(ir, *b_end))
                })
                .map(|end| (self.start, *end))
                .collect()
        } else if other.start.happens_before(ir, self.start) {
            other
                .ends
                .iter()
                .filter(|&b_end| self.start.happens_before(ir, *b_end))
                .filter(|&b_end| {
                    self.ends
                        .iter()
                        .any(|a_end| b_end.happens_before(ir, *a_end))
                })
                .map(|end| (other.start, *end))
                .collect()
        } else {
            HashSet::new()
        }
    }
}

#[derive(Default, Debug, Clone)]
pub struct BlockLifetimes {
    pub ordered_by_start: Vec<Binding>,
    pub binding_starts: HashMap<Binding, usize>,
    pub binding_ends: HashMap<Binding, usize>,
}

pub fn make_sorted_lifetimes(ir: &IR) -> Vec<BlockLifetimes> {
    let mut lasting_lifetimes = vec![HashSet::new(); ir.code.len()].into_boxed_slice();
    let mut collected = Vec::with_capacity(ir.code.len());
    for block in analysis::TopBottomTraversal::from(ir) {
        // collect our
        let mut this_block_lifetimes = analysis::antecessors(ir, block)
            .flat_map(|block| lasting_lifetimes[block.0].iter().copied())
            .collect();

        // calculate the block's local lifetimes.
        let mut calcd_lifetimes =
            make_block_lifetimes(&ir[block].statements, &mut this_block_lifetimes);

        lasting_lifetimes[block.0] = this_block_lifetimes;

        collected.push(calcd_lifetimes);
    }
    collected
}

// NOTE: this doesn't take into account lifetimes that go through a block, but are not used in that
// block.
// We'll have to take into account those by keeping track of what values are alive on each block
// and if they end, remove that "aliveness" for the rest of the branch.... For that I'll need to
// look at all the block's antecessors and any value that hasn't found and end
fn make_block_lifetimes(block: &[Statement], still_alive: &mut HashSet<Binding>) -> BlockLifetimes {
    let mut result: BlockLifetimes = Default::default();
    tracing::info!(target: "lifetimes::block", "filling in lifetime information");

    for (statement, stmt) in block.iter().enumerate() {
        use analysis::BindingUsage;

        // if the value is not a phi node, set the starts of these values if they haven't started
        // here.
        if let Statement::Assign { index, value } = stmt {
            // visit first the value bindings so that any values that are used here start before
            // the target is assigned.
            value.visit_value_bindings(|visited| {
                tracing::trace!(target: "lifetimes::block", "starts here: {visited}");
                if !result.ordered_by_start.contains(&visited) {
                    result.ordered_by_start.push(visited);
                }
                result.binding_starts.entry(visited).or_insert(0);
                result.binding_ends.insert(visited, block.len());
                ControlFlow::<(), ()>::Continue(())
            });

            tracing::trace!(target: "lifetimes::block", "starts here: {index}");
            result.ordered_by_start.push(*index);
            result.binding_starts.insert(*index, statement);
            result.binding_ends.insert(*index, block.len()); // assume it spans the whole block
                                                             // unless proven otherwise.
        }

        stmt.visit_value_bindings(|visited| {
            tracing::trace!(target: "lifetimes::block", "ends here: {visited}");
            // NOTE: we're only storing the last encounter of each visited binding. This way we ensure
            still_alive.remove(&visited);
            result.binding_ends.insert(visited, statement);
            ControlFlow::<(), ()>::Continue(())
        });
    }

    // for any still alive binding, we'll add a start and an end to it since they pass through this
    // block.
    for alive in still_alive.iter().copied() {
        result.ordered_by_start.push(alive);
        result.binding_starts.insert(alive, 0);
        result.binding_ends.insert(alive, block.len());
    }

    // NOTE :needs stable sort since the order that we visited the bindings in matters.
    result
        .ordered_by_start
        .sort_by_key(|binding| result.binding_starts[binding]);

    result
}

#[derive(Clone, Debug)]
pub struct Lifetime {
    pub attached_binding: Binding,
    pub start: BlockAddress,
    pub ends: Vec<BlockAddress>, // "actually" this should be just one end, since a lifetime is
                                 // just a code span. But since each binding
                                 // has just one start and more than one end, it's more useful to
                                 // put all those in.
}
impl std::fmt::Debug for BlockAddress {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}[{}]", self.block, self.statement)
    }
}

// TODO: move `Debug` impls to a separate module
#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct BlockAddress {
    pub block: BlockBinding,
    pub statement: usize,
}

impl BlockAddress {
    pub fn happens_between(self: Self, ir: &IR, a: Self, b: Self) -> bool {
        a.happens_before(ir, self) && self.happens_before(ir, b)
    }
    pub fn happens_before(self: Self, ir: &IR, other: Self) -> bool {
        // A happens before B if:
        // - the block that B happens in has A's block as a predecessor.
        // or:
        // - it happens before B in the same block
        (self.block == other.block && self.statement < other.statement)
            || analysis::antecessors(ir, other.block).any(|a| a == self.block)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::intermediate::*;
    mod block_address {
        use super::*;

        #[test]
        fn happens_before_same_block() {
            let ir = IR::from(vec![BasicBlock {
                statements: vec![
                    Statement::Assign {
                        index: Binding(0),
                        value: Value::Constant(2),
                    },
                    Statement::Assign {
                        index: Binding(1),
                        value: Value::Constant(3),
                    },
                    Statement::Assign {
                        index: Binding(2),
                        value: Value::Multiply {
                            lhs: Binding(0),
                            rhs: Binding(1).into(),
                        },
                    },
                ],
                end: BlockEnd::Return(Binding(2)),
            }]);

            let defs: HashMap<_, _> = get_defs(&ir).collect();

            assert!(defs[&Binding(0)].happens_before(&ir, defs[&Binding(1)]));
            assert!(!defs[&Binding(1)].happens_before(&ir, defs[&Binding(0)]));
        }

        #[test]
        fn happens_before_loop() {
            let ir = IR::from(vec![
                BasicBlock {
                    statements: vec![Statement::Assign {
                        index: Binding(0),
                        value: Value::Constant(2),
                    }],
                    end: Branch::Unconditional {
                        target: BlockBinding(1),
                    }
                    .into(),
                },
                BasicBlock {
                    statements: vec![Statement::Assign {
                        index: Binding(1),
                        value: Value::Constant(3),
                    }],
                    end: Branch::Unconditional {
                        target: BlockBinding(0),
                    }
                    .into(),
                },
            ]);

            let defs: HashMap<_, _> = get_defs(&ir).collect();

            assert!(defs[&Binding(0)].happens_before(&ir, defs[&Binding(1)]));
            assert!(defs[&Binding(1)].happens_before(&ir, defs[&Binding(0)]));
        }
    }
    #[test]
    fn collide_different_blocks() {
        // have binding %0 and %1 allocate. In different blocks that are related through a loop.
        // Write 0 to %0. Write 1 to %1. On a loop: Read a temp from %0. Read the value of %1 and
        // store the sum of it and the temp in %0. Write the temp in %1. Yes, this is fibonacci.
        // forever fibonacci. %0 and %1's memory lifetimes MUST collide.
        let ir = IR::from(vec![
            BasicBlock {
                // setup: declare %0 and %1, write 0 to %0 and 1 to %1.
                statements: vec![
                    Statement::Assign {
                        index: Binding(0),
                        value: Value::Allocate { size: 4 },
                    },
                    Statement::Assign {
                        index: Binding(1),
                        value: Value::Allocate { size: 4 },
                    },
                    Statement::Assign {
                        index: Binding(2),
                        value: 0.into(),
                    },
                    Statement::Store {
                        mem_binding: Binding(0),
                        binding: Binding(2),
                        byte_size: ByteSize::U32,
                    },
                    Statement::Assign {
                        index: Binding(3),
                        value: 1.into(),
                    },
                    Statement::Store {
                        mem_binding: Binding(1),
                        binding: Binding(3),
                        byte_size: ByteSize::U32,
                    },
                ],
                end: BlockEnd::Branch(Branch::Unconditional {
                    target: BlockBinding(1),
                }),
            },
            BasicBlock {
                statements: vec![
                    Statement::Assign {
                        index: Binding(4), // %4 will be our temp
                        value: Value::Load {
                            mem_binding: Binding(0),
                            byte_size: ByteSize::U32,
                        },
                    },
                    Statement::Assign {
                        index: Binding(5),
                        value: Value::Load {
                            mem_binding: Binding(1),
                            byte_size: ByteSize::U32,
                        },
                    },
                    Statement::Assign {
                        index: Binding(6),
                        value: Value::Add {
                            lhs: Binding(4),
                            rhs: Binding(5).into(),
                        },
                    },
                    Statement::Store {
                        mem_binding: Binding(1),
                        binding: Binding(6),
                        byte_size: ByteSize::U32,
                    },
                    Statement::Store {
                        mem_binding: Binding(0),
                        binding: Binding(4),
                        byte_size: ByteSize::U32,
                    },
                ],
                end: BlockEnd::Branch(Branch::Unconditional {
                    target: BlockBinding(1), // forever.
                }),
            },
        ]);
        dbg!(&ir.backwards_map);

        use crate::allocators::memory;
        let lifetimes = memory::compute_memory_lifetimes(&ir, &memory::make_alloc_map(&ir.code));

        assert!(BlockAddress {
            block: BlockBinding(1),
            statement: 3
        }
        .happens_before(
            &ir,
            BlockAddress {
                block: BlockBinding(1),
                statement: 0
            }
        ));

        let collisions = compute_lifetime_collisions(&ir, &lifetimes);

        dbg!(&ir);

        assert!(collisions[&Binding(1)].contains(&Binding(0)));
    }

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
            ends: vec![BlockAddress {
                block: BlockBinding(0),
                statement: 0,
            }],
            start: BlockAddress {
                block: BlockBinding(0),
                statement: 2,
            },
        };
        let lifetime_2 = Lifetime {
            attached_binding: Binding(2),
            ends: vec![BlockAddress {
                block: BlockBinding(0),
                statement: 3,
            }],
            start: BlockAddress {
                block: BlockBinding(0),
                statement: 4,
            },
        };
        assert!(lifetime_1.find_intersections(&lifetime_2, &ir).is_empty());
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

        assert!(lifetimes[0]
            .find_intersections(&lifetimes[1], &ir)
            .is_empty());
        assert!(lifetimes[0]
            .find_intersections(&lifetimes[2], &ir)
            .is_empty());

        assert!(lifetimes[1]
            .find_intersections(&lifetimes[2], &ir)
            .is_empty());
        assert!(lifetimes[1]
            .find_intersections(&lifetimes[0], &ir)
            .is_empty());

        assert!(lifetimes[2]
            .find_intersections(&lifetimes[1], &ir)
            .is_empty());
        assert!(lifetimes[2]
            .find_intersections(&lifetimes[0], &ir)
            .is_empty());
    }

    fn compile_source_into_ir(source: &str) -> anyhow::Result<crate::intermediate::IR> {
        let meta = crate::error::SourceMetadata::new(source).with_file("<test program>".into());
        let program = crate::grammar::Parser::new(&meta).parse()?;
        let (_function_name, ir) = crate::intermediate::generate::compile_function(program, &meta)?;
        Ok(ir)
    }

    #[test]
    fn correct_branch_pass_through() -> anyhow::Result<()> {
        const SOURCE_CODE: &str = include_str!(
            "../../../write_a_c_compiler/stage_4/valid/skip_on_failure_multi_short_circuit.c"
        );

        let ir = compile_source_into_ir(SOURCE_CODE)?;
        let lifetimes = analysis::compute_lifetimes(&ir);
        let collision_map = analysis::compute_lifetime_collisions(&ir, &lifetimes);

        dbg!(&lifetimes);

        assert!(
            !collision_map[&Binding(2)].contains(&Binding(3)),
            "{:?}\n%2 and %3 should *NOT* collide",
            ir
        );

        Ok(())
    }
}
