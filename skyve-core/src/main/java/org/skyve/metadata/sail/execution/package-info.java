/**
 * Defines the {@link org.skyve.metadata.sail.execution.Executor} contract for executing
 * SAIL (Skyve Automated Interface Language) automation scripts.
 *
 * <p>SAIL is Skyve's UI automation DSL. An {@code Executor} implements the Visitor
 * pattern over the SAIL language model — each {@code execute*()} method corresponds
 * to a concrete {@link org.skyve.metadata.sail.language.Step} subtype. This separation
 * keeps the language model free of execution concerns, allowing multiple executor
 * implementations (browser-based, mock, generator) without changing the script structure.
 *
 * @see org.skyve.metadata.sail.language
 */
package org.skyve.metadata.sail.execution;
