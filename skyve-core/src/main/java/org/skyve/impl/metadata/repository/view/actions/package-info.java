/**
 * JAXB-annotated metadata classes for view action declarations.
 *
 * <p>Each class in this package corresponds to one action element that can appear in a
 * view's {@code <actions>} section. They form the JAXB binding for the view action XML:
 * <ul>
 *   <li>{@code ActionMetaData} — abstract base carrying display name, icon, tooltip,
 *       and visibility condition.
 *   <li>{@code PositionableAction} — extends with toolbar position.
 *   <li>{@code ValidatableAction} — extends with client-side validation flag.
 *   <li>{@code ParameterizableAction} — extends with server-side parameters map.
 *   <li>Implicit actions: {@code SaveAction}, {@code CancelAction}, {@code DeleteAction},
 *       {@code OKAction}, {@code RemoveAction}, {@code ZoomOutAction}, {@code NewAction},
 *       {@code AddAction}, {@code DefaultsAction}.
 *   <li>Explicit/named actions: {@code ClassAction} (server-side class),
 *       {@code CustomAction} (client-side).
 *   <li>Report/file actions: {@code ReportAction}, {@code PrintAction},
 *       {@code BizExportAction}, {@code BizImportAction}, {@code DownloadAction},
 *       {@code UploadAction}.
 * </ul>
 *
 * @see org.skyve.impl.metadata.repository.view
 * @see org.skyve.metadata.controller.ImplicitActionName
 */
package org.skyve.impl.metadata.repository.view.actions;
