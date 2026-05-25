/**
 * SAIL steps that manage the automation execution context stack.
 *
 * <p>SAIL execution is context-sensitive: before interacting with a view the executor
 * must establish which module/document and view type (edit or list) it is operating on.
 * This package provides the context-management steps that control that stack:
 * <ul>
 *   <li>{@link org.skyve.metadata.sail.language.step.context.PushEditContext} — pushes a
 *       new edit-view context for the specified module and document onto the stack,
 *       optionally constraining the UX/UI and user-agent type.
 *   <li>{@link org.skyve.metadata.sail.language.step.context.PushListContext} — pushes a
 *       new list-view context for the specified module and document.
 *   <li>{@link org.skyve.metadata.sail.language.step.context.PopContext} — pops the
 *       innermost context from the stack, returning to the previous context.
 *   <li>{@link org.skyve.metadata.sail.language.step.context.ClearContext} — removes all
 *       contexts from the stack, resetting the executor to a clean state.
 * </ul>
 *
 * <p>Context steps are typically placed at the start and end of a logical test scenario.
 * Push steps must be balanced by either a corresponding pop or a clear, otherwise the
 * executor context stack leaks between scenarios.
 *
 * @see org.skyve.metadata.sail.language.step.interaction
 */
package org.skyve.metadata.sail.language.step.context;
