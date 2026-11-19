/**
 * Root-level SAIL step types: utilities, assertions, and inline execution.
 *
 * <p>This package contains the top-level {@link org.skyve.metadata.sail.language.Step}
 * implementations that are not tied to a specific UI interaction or context operation:
 * <ul>
 *   <li>{@link org.skyve.metadata.sail.language.step.Comment} — embeds a non-executable
 *       annotation in the script for human readers.
 *   <li>{@link org.skyve.metadata.sail.language.step.Execute} — runs an inline JavaScript
 *       fragment in the page, optionally keeping its result in a variable.
 *   <li>{@link org.skyve.metadata.sail.language.step.ExecuteAsync} — runs the body of an
 *       {@code async} JavaScript function in the page and waits for it to settle.
 *   <li>{@link org.skyve.metadata.sail.language.step.WaitFor} — waits until a JavaScript
 *       condition in the page holds.
 *   <li>{@link org.skyve.metadata.sail.language.step.TestScript} — asserts, in JavaScript,
 *       something about the page.
 *   <li>{@link org.skyve.metadata.sail.language.step.Call} — runs a handwritten Java step, a
 *       {@link org.skyve.metadata.sail.execution.SailCall}, with its
 *       {@link org.skyve.metadata.sail.language.step.CallParameter}s.
 *   <li>{@link org.skyve.metadata.sail.language.step.Pause} — waits for the specified
 *       number of milliseconds before continuing.
 *   <li>{@link org.skyve.metadata.sail.language.step.TestSuccess} — asserts that no error
 *       messages are visible in the current UI state.
 *   <li>{@link org.skyve.metadata.sail.language.step.TestFailure} — asserts that one or
 *       more error messages are visible, optionally matching a specific message text.
 *   <li>{@link org.skyve.metadata.sail.language.step.TestValue} — asserts that the widget
 *       bound to a given binding holds an expected value.
 *   <li>{@link org.skyve.metadata.sail.language.step.TestMessage} — asserts that a message
 *       containing some text, optionally of a given severity, is shown.
 *   <li>{@link org.skyve.metadata.sail.language.step.TestGridRows} — asserts how many rows,
 *       optionally containing some text, a list or data grid shows.
 *   <li>{@link org.skyve.metadata.sail.language.step.TestWidget} — asserts whether a widget,
 *       action button or tab is visible and enabled.
 *   <li>{@link org.skyve.metadata.sail.language.step.CaptureValue} — keeps the value a widget
 *       shows in a variable that later steps use as {@code {var:name}}.
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
