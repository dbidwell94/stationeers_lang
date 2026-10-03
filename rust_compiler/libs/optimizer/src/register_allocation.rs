use il::{DeviceReference, Instruction, InstructionNode, LiteralOrReference, Operand};
use std::collections::{BTreeSet, HashMap, HashSet};
use thiserror::Error;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ControlFlowGraph {
    pub successors: Vec<Vec<usize>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Liveness {
    pub live_in: Vec<HashSet<u32>>,
    pub live_out: Vec<HashSet<u32>>,
    pub live_across_calls: HashMap<usize, HashSet<u32>>,
    pub physical_live_in: Vec<HashSet<u8>>,
    pub physical_live_out: Vec<HashSet<u8>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RegisterAllocation {
    pub registers: HashMap<u32, u8>,
    pub spills: Vec<u32>,
}

#[derive(Debug, Error, PartialEq, Eq)]
pub enum AnalysisError {
    #[error("Control-flow target `{0}` is not defined.")]
    UnknownLabel(String),
    #[error("Control-flow target must be a symbolic label before label resolution.")]
    NonSymbolicTarget,
}

#[derive(Debug, Error, PartialEq, Eq)]
pub enum RewriteError {
    #[error("Register allocation requires spilling virtual registers {0:?}.")]
    SpillRequired(Vec<u32>),
    #[error("Virtual register v{0} has no physical-register assignment.")]
    MissingAssignment(u32),
}

pub fn build_control_flow_graph(
    instructions: &[InstructionNode<'_>],
) -> Result<ControlFlowGraph, AnalysisError> {
    let mut labels = HashMap::new();
    for (index, node) in instructions.iter().enumerate() {
        if let Instruction::LabelDef(label) = &node.instruction {
            labels.insert(label.as_ref(), index);
        }
    }

    let mut successors = vec![Vec::new(); instructions.len()];
    for (index, node) in instructions.iter().enumerate() {
        let fallthrough = (index + 1 < instructions.len()).then_some(index + 1);
        match &node.instruction {
            Instruction::Jump(Operand::ReturnAddress) => {}
            Instruction::Jump(target) => {
                successors[index].push(resolve_target(target, &labels)?);
            }
            Instruction::BranchEq(_, _, target)
            | Instruction::BranchNe(_, _, target)
            | Instruction::BranchGt(_, _, target)
            | Instruction::BranchLt(_, _, target)
            | Instruction::BranchGe(_, _, target)
            | Instruction::BranchLe(_, _, target)
            | Instruction::BranchEqZero(_, target)
            | Instruction::BranchNeZero(_, target) => {
                successors[index].push(resolve_target(target, &labels)?);
                if let Some(next) = fallthrough {
                    successors[index].push(next);
                }
            }
            Instruction::JumpRelative(_) => {}
            _ => {
                if let Some(next) = fallthrough {
                    successors[index].push(next);
                }
            }
        }
    }

    Ok(ControlFlowGraph { successors })
}

fn resolve_target(
    target: &Operand<'_>,
    labels: &HashMap<&str, usize>,
) -> Result<usize, AnalysisError> {
    let Operand::Label(label) = target else {
        return Err(AnalysisError::NonSymbolicTarget);
    };
    labels
        .get(label.as_ref())
        .copied()
        .ok_or_else(|| AnalysisError::UnknownLabel(label.to_string()))
}

pub fn analyze_liveness(
    instructions: &[InstructionNode<'_>],
) -> Result<(ControlFlowGraph, Liveness), AnalysisError> {
    let graph = build_control_flow_graph(instructions)?;
    let registers: BTreeSet<u32> = instructions
        .iter()
        .flat_map(|node| virtual_registers(&node.instruction))
        .collect();

    let uses: Vec<HashSet<u32>> = instructions
        .iter()
        .map(|node| {
            registers
                .iter()
                .copied()
                .filter(|register| {
                    super::helpers::virtual_reg_is_read(&node.instruction, *register)
                })
                .collect()
        })
        .collect();
    let definitions: Vec<Option<u32>> = instructions
        .iter()
        .map(|node| super::helpers::get_virtual_destination_reg(&node.instruction))
        .collect();
    let physical_uses: Vec<HashSet<u8>> = instructions
        .iter()
        .map(|node| {
            (0..=15)
                .filter(|register| super::helpers::reg_is_read(&node.instruction, *register))
                .collect()
        })
        .collect();
    let physical_definitions: Vec<Option<u8>> = instructions
        .iter()
        .map(|node| super::helpers::get_destination_reg(&node.instruction))
        .collect();

    let mut live_in = vec![HashSet::new(); instructions.len()];
    let mut live_out = vec![HashSet::new(); instructions.len()];
    let mut physical_live_in = vec![HashSet::new(); instructions.len()];
    let mut physical_live_out = vec![HashSet::new(); instructions.len()];
    let mut changed = true;
    while changed {
        changed = false;
        for index in (0..instructions.len()).rev() {
            let next_out: HashSet<u32> = graph.successors[index]
                .iter()
                .flat_map(|successor| live_in[*successor].iter().copied())
                .collect();
            let mut next_in = uses[index].clone();
            next_in.extend(
                next_out
                    .iter()
                    .copied()
                    .filter(|register| definitions[index] != Some(*register)),
            );
            let next_physical_out: HashSet<u8> = graph.successors[index]
                .iter()
                .flat_map(|successor| physical_live_in[*successor].iter().copied())
                .collect();
            let mut next_physical_in = physical_uses[index].clone();
            next_physical_in.extend(
                next_physical_out
                    .iter()
                    .copied()
                    .filter(|register| physical_definitions[index] != Some(*register)),
            );
            if next_out != live_out[index]
                || next_in != live_in[index]
                || next_physical_out != physical_live_out[index]
                || next_physical_in != physical_live_in[index]
            {
                live_out[index] = next_out;
                live_in[index] = next_in;
                physical_live_out[index] = next_physical_out;
                physical_live_in[index] = next_physical_in;
                changed = true;
            }
        }
    }

    let live_across_calls = instructions
        .iter()
        .enumerate()
        .filter(|(_, node)| matches!(node.instruction, Instruction::JumpAndLink(_)))
        .map(|(index, _)| (index, live_out[index].clone()))
        .collect();

    Ok((
        graph,
        Liveness {
            live_in,
            live_out,
            live_across_calls,
            physical_live_in,
            physical_live_out,
        },
    ))
}

pub fn allocate_registers(
    instructions: &[InstructionNode<'_>],
) -> Result<RegisterAllocation, AnalysisError> {
    let (_, liveness) = analyze_liveness(instructions)?;
    let mut interference: HashMap<u32, BTreeSet<u32>> = HashMap::new();
    let mut move_preferences: HashMap<u32, BTreeSet<u32>> = HashMap::new();
    let mut move_sources = HashSet::new();
    let mut forbidden_registers: HashMap<u32, HashSet<u8>> = HashMap::new();

    for node in instructions {
        for register in virtual_registers(&node.instruction) {
            interference.entry(register).or_default();
        }
        if let Instruction::Move(
            Operand::VirtualRegister(destination),
            Operand::VirtualRegister(source),
        ) = &node.instruction
            && destination != source
        {
            move_sources.insert(*source);
            move_preferences
                .entry(*destination)
                .or_default()
                .insert(*source);
            move_preferences
                .entry(*source)
                .or_default()
                .insert(*destination);
        }
    }

    for (index, node) in instructions.iter().enumerate() {
        let mut fixed_registers = liveness.physical_live_in[index].clone();
        fixed_registers.extend(liveness.physical_live_out[index].iter().copied());
        fixed_registers.extend((0..=15).filter(|register| {
            super::helpers::reg_is_read(&node.instruction, *register)
                || super::helpers::get_destination_reg(&node.instruction) == Some(*register)
        }));
        let mut virtual_registers_here: HashSet<u32> = liveness.live_in[index].clone();
        virtual_registers_here.extend(liveness.live_out[index].iter().copied());
        virtual_registers_here.extend(virtual_registers(&node.instruction));
        for virtual_register in virtual_registers_here {
            forbidden_registers
                .entry(virtual_register)
                .or_default()
                .extend(fixed_registers.iter().copied());
        }

        let Some(destination) = super::helpers::get_virtual_destination_reg(&node.instruction)
        else {
            continue;
        };
        for live_register in &liveness.live_out[index] {
            if *live_register != destination {
                interference
                    .entry(destination)
                    .or_default()
                    .insert(*live_register);
                interference
                    .entry(*live_register)
                    .or_default()
                    .insert(destination);
            }
        }
    }

    Ok(color_graph(
        interference,
        &(1..=14).collect::<Vec<_>>(),
        forbidden_registers,
        move_preferences,
        move_sources,
    ))
}

pub fn rewrite_registers<'a>(
    mut instructions: il::Instructions<'a>,
    allocation: &RegisterAllocation,
) -> Result<il::Instructions<'a>, RewriteError> {
    if !allocation.spills.is_empty() {
        return Err(RewriteError::SpillRequired(allocation.spills.clone()));
    }

    for node in instructions.iter_mut() {
        let mut missing = None;
        node.instruction.visit_operands_mut(|operand| {
            let virtual_id = match operand {
                Operand::VirtualRegister(id) => Some(*id),
                Operand::DeviceReference(reference) => match reference {
                    DeviceReference::Housing(LiteralOrReference::VirtualReference(id))
                    | DeviceReference::Pin(LiteralOrReference::VirtualReference(id))
                    | DeviceReference::Reference(LiteralOrReference::VirtualReference(id)) => {
                        Some(*id)
                    }
                    _ => None,
                },
                _ => None,
            };
            if let Some(id) = virtual_id {
                if let Some(register) = allocation.registers.get(&id) {
                    match operand {
                        Operand::VirtualRegister(_) => *operand = Operand::Register(*register),
                        Operand::DeviceReference(reference) => match reference {
                            DeviceReference::Housing(value)
                            | DeviceReference::Pin(value)
                            | DeviceReference::Reference(value) => {
                                *value = LiteralOrReference::Reference(*register);
                            }
                        },
                        _ => unreachable!(),
                    }
                } else {
                    missing = Some(id);
                }
            }
        });
        if let Some(id) = missing {
            return Err(RewriteError::MissingAssignment(id));
        }
    }

    instructions.retain(|node| {
        !matches!(&node.instruction, Instruction::Move(destination, source) if destination == source)
    });

    Ok(instructions)
}

fn virtual_registers(instruction: &Instruction<'_>) -> Vec<u32> {
    let mut registers = Vec::new();
    instruction.visit_operands(|operand| match operand {
        Operand::VirtualRegister(id) => registers.push(*id),
        Operand::DeviceReference(reference) => {
            let value = match reference {
                DeviceReference::Housing(value)
                | DeviceReference::Pin(value)
                | DeviceReference::Reference(value) => value,
            };
            if let LiteralOrReference::VirtualReference(id) = value {
                registers.push(*id);
            }
        }
        _ => {}
    });
    registers
}

fn color_graph(
    graph: HashMap<u32, BTreeSet<u32>>,
    available_registers: &[u8],
    forbidden_registers: HashMap<u32, HashSet<u8>>,
    move_preferences: HashMap<u32, BTreeSet<u32>>,
    move_sources: HashSet<u32>,
) -> RegisterAllocation {
    let mut remaining = graph.clone();
    let mut stack = Vec::with_capacity(remaining.len());

    while !remaining.is_empty() {
        let low_degree = remaining
            .iter()
            .filter(|(_, neighbors)| {
                neighbors
                    .iter()
                    .filter(|neighbor| remaining.contains_key(neighbor))
                    .count()
                    < available_registers.len()
            })
            .map(|(register, _)| *register)
            .min_by_key(|register| (!move_sources.contains(register), *register));
        let selected = low_degree.unwrap_or_else(|| {
            remaining
                .iter()
                .max_by_key(|(register, neighbors)| {
                    (
                        neighbors
                            .iter()
                            .filter(|neighbor| remaining.contains_key(neighbor))
                            .count(),
                        std::cmp::Reverse(**register),
                    )
                })
                .map(|(register, _)| *register)
                .expect("remaining graph is nonempty")
        });
        remaining.remove(&selected);
        stack.push(selected);
    }

    let mut registers = HashMap::new();
    let mut spills = Vec::new();
    while let Some(register) = stack.pop() {
        let unavailable: HashSet<u8> = graph[&register]
            .iter()
            .filter_map(|neighbor| registers.get(neighbor).copied())
            .collect();
        let color_is_available = |color: &u8| {
            !unavailable.contains(color)
                && !forbidden_registers
                    .get(&register)
                    .is_some_and(|forbidden| forbidden.contains(color))
        };
        let preferred_color = move_preferences
            .get(&register)
            .into_iter()
            .flatten()
            .filter_map(|preferred| registers.get(preferred))
            .find(|color| color_is_available(color));
        let color = preferred_color.copied().or_else(|| {
            available_registers
                .iter()
                .find(|color| color_is_available(color))
                .copied()
        });

        if let Some(color) = color {
            registers.insert(register, color);
        } else {
            spills.push(register);
        }
    }
    spills.sort_unstable();
    RegisterAllocation { registers, spills }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn node(instruction: Instruction<'static>) -> InstructionNode<'static> {
        InstructionNode::new(instruction, None)
    }

    #[test]
    fn cfg_includes_both_branch_edges_and_unconditional_jump_target() {
        let instructions = vec![
            node(Instruction::BranchEqZero(
                Operand::Register(1),
                Operand::Label("done".into()),
            )),
            node(Instruction::Move(
                Operand::Register(1),
                Operand::Number(1.into()),
            )),
            node(Instruction::Jump(Operand::Label("done".into()))),
            node(Instruction::LabelDef("done".into())),
            node(Instruction::Jump(Operand::ReturnAddress)),
        ];

        let graph = build_control_flow_graph(&instructions).unwrap();

        assert_eq!(graph.successors[0], vec![3, 1]);
        assert_eq!(graph.successors[2], vec![3]);
        assert!(graph.successors[4].is_empty());
    }

    #[test]
    fn liveness_reaches_a_fixed_point_across_a_loop_backedge() {
        let instructions = vec![
            node(Instruction::Move(
                Operand::VirtualRegister(1),
                Operand::Number(0.into()),
            )),
            node(Instruction::LabelDef("loop".into())),
            node(Instruction::BranchEqZero(
                Operand::VirtualRegister(2),
                Operand::Label("done".into()),
            )),
            node(Instruction::Add(
                Operand::VirtualRegister(1),
                Operand::VirtualRegister(1),
                Operand::Number(1.into()),
            )),
            node(Instruction::Jump(Operand::Label("loop".into()))),
            node(Instruction::LabelDef("done".into())),
            node(Instruction::Push(Operand::VirtualRegister(1))),
            node(Instruction::JumpRelative(Operand::ReturnAddress)),
        ];

        let (graph, liveness) = analyze_liveness(&instructions).unwrap();

        assert_eq!(graph.successors[4], vec![1]);
        assert!(liveness.live_in[1].contains(&1));
        assert!(liveness.live_out[3].contains(&1));
    }

    #[test]
    fn liveness_identifies_virtual_values_used_after_a_call() {
        let instructions = vec![
            node(Instruction::Move(
                Operand::VirtualRegister(6),
                Operand::Number(3.into()),
            )),
            node(Instruction::JumpAndLink(Operand::Label("callee".into()))),
            node(Instruction::Push(Operand::VirtualRegister(6))),
            node(Instruction::Jump(Operand::Label("done".into()))),
            node(Instruction::LabelDef("callee".into())),
            node(Instruction::JumpRelative(Operand::ReturnAddress)),
            node(Instruction::LabelDef("done".into())),
            node(Instruction::JumpRelative(Operand::ReturnAddress)),
        ];

        let (_, liveness) = analyze_liveness(&instructions).unwrap();

        assert_eq!(liveness.live_across_calls[&1], HashSet::from([6]));
    }

    #[test]
    fn indirect_device_reference_participates_in_liveness_and_rewriting() {
        let instructions = vec![
            node(Instruction::Move(
                Operand::VirtualRegister(9),
                Operand::Number(0.into()),
            )),
            node(Instruction::Load(
                Operand::Register(1),
                Operand::DeviceReference(DeviceReference::Pin(
                    LiteralOrReference::VirtualReference(9),
                )),
                Operand::LogicType("On".into()),
            )),
        ];

        let allocation = allocate_registers(&instructions).unwrap();
        let rewritten =
            rewrite_registers(il::Instructions::new(instructions), &allocation).unwrap();
        let assigned_register = allocation.registers[&9];

        assert_ne!(assigned_register, 1);
        assert_eq!(
            rewritten[1].instruction,
            Instruction::Load(
                Operand::Register(1),
                Operand::DeviceReference(DeviceReference::Pin(LiteralOrReference::Reference(
                    assigned_register,
                ))),
                Operand::LogicType("On".into()),
            )
        );
    }

    #[test]
    fn noninterfering_virtual_registers_reuse_a_hardware_register() {
        let instructions = vec![
            node(Instruction::Move(
                Operand::VirtualRegister(1),
                Operand::Number(1.into()),
            )),
            node(Instruction::Push(Operand::VirtualRegister(1))),
            node(Instruction::Move(
                Operand::VirtualRegister(2),
                Operand::Number(2.into()),
            )),
            node(Instruction::Push(Operand::VirtualRegister(2))),
        ];

        let allocation = allocate_registers(&instructions).unwrap();

        assert_eq!(allocation.registers[&1], allocation.registers[&2]);
        assert!(allocation.spills.is_empty());
    }

    #[test]
    fn simultaneously_live_values_receive_distinct_registers() {
        let instructions = vec![
            node(Instruction::Move(
                Operand::VirtualRegister(1),
                Operand::Number(1.into()),
            )),
            node(Instruction::Move(
                Operand::VirtualRegister(2),
                Operand::Number(2.into()),
            )),
            node(Instruction::Add(
                Operand::VirtualRegister(3),
                Operand::VirtualRegister(1),
                Operand::VirtualRegister(2),
            )),
            node(Instruction::Push(Operand::VirtualRegister(3))),
        ];

        let allocation = allocate_registers(&instructions).unwrap();

        assert_ne!(allocation.registers[&1], allocation.registers[&2]);
        assert!(allocation.spills.is_empty());
    }

    #[test]
    fn virtual_values_do_not_overlap_live_physical_registers() {
        let instructions = vec![
            node(Instruction::Move(
                Operand::Register(1),
                Operand::Number(5.into()),
            )),
            node(Instruction::Move(
                Operand::VirtualRegister(1),
                Operand::Number(7.into()),
            )),
            node(Instruction::Add(
                Operand::VirtualRegister(2),
                Operand::VirtualRegister(1),
                Operand::Register(1),
            )),
            node(Instruction::Push(Operand::VirtualRegister(2))),
        ];

        let allocation = allocate_registers(&instructions).unwrap();

        assert_ne!(allocation.registers[&1], 1);
        assert!(allocation.spills.is_empty());
    }

    #[test]
    fn rewrite_replaces_virtual_operands_with_physical_registers() {
        let instructions = il::Instructions::new(vec![
            node(Instruction::Move(
                Operand::VirtualRegister(4),
                Operand::Number(9.into()),
            )),
            node(Instruction::Push(Operand::VirtualRegister(4))),
        ]);
        let allocation = RegisterAllocation {
            registers: HashMap::from([(4, 7)]),
            spills: Vec::new(),
        };

        let rewritten = rewrite_registers(instructions, &allocation).unwrap();

        assert_eq!(
            rewritten[0].instruction,
            Instruction::Move(Operand::Register(7), Operand::Number(9.into()))
        );
        assert_eq!(
            rewritten[1].instruction,
            Instruction::Push(Operand::Register(7))
        );
    }

    #[test]
    fn move_affinity_coalesces_noninterfering_values_and_removes_the_copy() {
        let instructions = vec![
            node(Instruction::Move(
                Operand::VirtualRegister(2),
                Operand::Number(5.into()),
            )),
            node(Instruction::Move(
                Operand::VirtualRegister(1),
                Operand::VirtualRegister(2),
            )),
            node(Instruction::Push(Operand::VirtualRegister(1))),
        ];

        let allocation = allocate_registers(&instructions).unwrap();
        assert_eq!(allocation.registers[&1], allocation.registers[&2]);

        let rewritten =
            rewrite_registers(il::Instructions::new(instructions), &allocation).unwrap();

        assert!(!rewritten.iter().any(|node| {
            matches!(
                &node.instruction,
                Instruction::Move(Operand::Register(destination), Operand::Register(source))
                    if destination == source
            )
        }));
    }

    #[test]
    fn move_affinity_does_not_coalesce_interfering_values() {
        let instructions = vec![
            node(Instruction::Move(
                Operand::VirtualRegister(1),
                Operand::Number(5.into()),
            )),
            node(Instruction::Move(
                Operand::VirtualRegister(2),
                Operand::VirtualRegister(1),
            )),
            node(Instruction::Add(
                Operand::VirtualRegister(3),
                Operand::VirtualRegister(1),
                Operand::VirtualRegister(2),
            )),
            node(Instruction::Push(Operand::VirtualRegister(3))),
        ];

        let allocation = allocate_registers(&instructions).unwrap();

        assert_ne!(allocation.registers[&1], allocation.registers[&2]);
    }

    #[test]
    fn rewrite_refuses_to_drop_spills_or_unassigned_virtuals() {
        let instructions =
            il::Instructions::new(vec![node(Instruction::Push(Operand::VirtualRegister(8)))]);
        let spills = RegisterAllocation {
            registers: HashMap::new(),
            spills: vec![8],
        };
        assert!(matches!(
            rewrite_registers(instructions, &spills),
            Err(RewriteError::SpillRequired(registers)) if registers == vec![8]
        ));

        let instructions =
            il::Instructions::new(vec![node(Instruction::Push(Operand::VirtualRegister(8)))]);
        let missing = RegisterAllocation {
            registers: HashMap::new(),
            spills: Vec::new(),
        };
        assert!(matches!(
            rewrite_registers(instructions, &missing),
            Err(RewriteError::MissingAssignment(8))
        ));
    }
}
