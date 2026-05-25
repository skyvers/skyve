/**
 * JAXB-annotated metadata classes for view user-access declarations.
 *
 * <p>Skyve view XML files can declare which Skyve resources a user must be able to
 * access for the view to be rendered. The classes here bind the
 * {@code <userAccesses>} XML element and its children:
 * <ul>
 *   <li>{@code ViewUserAccessesMetaData} — container element {@code <userAccesses>}.
 *   <li>{@code ViewUserAccessMetaData} — abstract base for a single access declaration.
 *   <li>{@code ViewUserAccessUxUiMetadata} — UX/UI-specific access override.
 *   <li>{@code ViewSingularUserAccessMetaData} — access to a specific document
 *       instance (edit permission).
 *   <li>{@code ViewDocumentAggregateUserAccessMetaData} — access to a document's list
 *       or aggregate (read permission).
 *   <li>{@code ViewQueryAggregateUserAccessMetaData} — access to a named query.
 *   <li>{@code ViewModelAggregateUserAccessMetaData} — access to a list model.
 *   <li>{@code ViewContentUserAccessMetaData} — access to a content attachment.
 *   <li>{@code ViewDynamicImageUserAccessMetaData} — access to a dynamic image.
 *   <li>{@code ViewReportUserAccessMetaData} — access to a report.
 *   <li>{@code ViewPreviousCompleteUserAccessMetaData} — access that depends on
 *       previous workflow step completion.
 * </ul>
 *
 * @see org.skyve.impl.metadata.repository.view
 */
package org.skyve.impl.metadata.repository.view.access;
