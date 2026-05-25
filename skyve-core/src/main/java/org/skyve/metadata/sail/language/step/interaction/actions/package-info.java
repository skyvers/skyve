/**
 * SAIL steps that fire Skyve action buttons within the current view.
 *
 * <p>This package contains one class per implicit action, plus a base class and
 * a general-purpose named-action step:
 * <ul>
 *   <li>{@link org.skyve.metadata.sail.language.step.interaction.actions.AbstractAction} —
 *       base class that adds an optional {@code testSuccess} flag; when {@code true} (the
 *       default) the executor automatically asserts that no errors appear after firing the
 *       action.
 *   <li>{@link org.skyve.metadata.sail.language.step.interaction.actions.Action} — fires a
 *       named (explicit) action, such as BizImport, BizExport, Upload, Download, or Report.
 *   <li>{@link org.skyve.metadata.sail.language.step.interaction.actions.Save} — fires the
 *       {@link org.skyve.metadata.controller.ImplicitActionName#Save} implicit action.
 *   <li>{@link org.skyve.metadata.sail.language.step.interaction.actions.Cancel} — fires
 *       {@link org.skyve.metadata.controller.ImplicitActionName#Cancel}.
 *   <li>{@link org.skyve.metadata.sail.language.step.interaction.actions.Delete} — fires
 *       {@link org.skyve.metadata.controller.ImplicitActionName#Delete}.
 *   <li>{@link org.skyve.metadata.sail.language.step.interaction.actions.Ok} — fires
 *       {@link org.skyve.metadata.controller.ImplicitActionName#OK}.
 *   <li>{@link org.skyve.metadata.sail.language.step.interaction.actions.Remove} — fires
 *       {@link org.skyve.metadata.controller.ImplicitActionName#Remove}.
 *   <li>{@link org.skyve.metadata.sail.language.step.interaction.actions.ZoomIn} — fires a
 *       zoom-in action, navigating into a related document.
 *   <li>{@link org.skyve.metadata.sail.language.step.interaction.actions.ZoomOut} — fires
 *       {@link org.skyve.metadata.controller.ImplicitActionName#ZoomOut}.
 * </ul>
 *
 * @see org.skyve.metadata.sail.language.step.interaction
 * @see org.skyve.metadata.controller.ImplicitActionName
 */
package org.skyve.metadata.sail.language.step.interaction.actions;
