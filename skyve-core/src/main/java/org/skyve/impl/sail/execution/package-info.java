/**
 * SAIL (Skyve Automated Interface Language) script execution contexts and executors.
 *
 * <p>This package provides the runtime support for executing SAIL interaction scripts
 * against the Skyve application. SAIL scripts are used for automated UI testing and
 * scripted UI generation (Flutter, React client code generation).
 *
 * <p>Key types:
 * <ul>
 *   <li>{@code AutomationContext} — carries state for an executing SAIL session,
 *       including the current document, action results, and accumulated navigation path.
 *   <li>{@code ContextualExecutor} — executes a SAIL step within a given context;
 *       the base class for all execution modes.
 *   <li>{@code ScriptExecutor} — drives a full SAIL script from start to finish,
 *       managing context transitions between steps.
 *   <li>{@code GenerateEditContext} / {@code GenerateListContext} — specialised
 *       execution contexts used during client code generation to simulate edit and
 *       list view navigation.
 * </ul>
 *
 * @see org.skyve.metadata.sail
 */
package org.skyve.impl.sail.execution;
