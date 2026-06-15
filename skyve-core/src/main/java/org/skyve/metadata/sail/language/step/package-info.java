/**
 * Root-level SAIL step types: utilities, assertions, and inline execution.
 *
 * <p>This package contains the top-level {@link org.skyve.metadata.sail.language.Step}
 * implementations that are not tied to a specific UI interaction or context operation:
 * <ul>
 *   <li>{@link org.skyve.metadata.sail.language.step.Comment} — embeds a non-executable
 *       annotation in the script for human readers.
 *   <li>{@link org.skyve.metadata.sail.language.step.Execute} — runs an inline Groovy/script
 *       fragment during automation execution.
 *   <li>{@link org.skyve.metadata.sail.language.step.Pause} — waits for the specified
 *       number of milliseconds before continuing.
 *   <li>{@link org.skyve.metadata.sail.language.step.TestSuccess} — asserts that no error
 *       messages are visible in the current UI state.
 *   <li>{@link org.skyve.metadata.sail.language.step.TestFailure} — asserts that one or
 *       more error messages are visible, optionally matching a specific message text.
 *   <li>{@link org.skyve.metadata.sail.language.step.TestValue} — asserts that the widget
 *       bound to a given binding holds an expected value.
 * </ul>
 *
 * <p>All types are JAXB-annotated for XML serialisation using the namespace defined by
 * {@code org.skyve.impl.util.XMLMetaData#SAIL_NAMESPACE}.
 *
 * @see org.skyve.metadata.sail.language.Step
 * @see org.skyve.metadata.sail.language.step.context
 * @see org.skyve.metadata.sail.language.step.interaction
 */
package org.skyve.metadata.sail.language.step;
