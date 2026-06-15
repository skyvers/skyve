/**
 * Content management API: storage, retrieval, and full-text search of managed content.
 *
 * <p>The central service is {@link org.skyve.content.ContentManager}, obtained via
 * {@link org.skyve.EXT#newContentManager()}. It manages two categories:
 * <ul>
 *   <li>{@link org.skyve.content.AttachmentContent} — binary file data attached to
 *       document attributes; stored by content ID and optionally full-text indexed.</li>
 *   <li>{@link org.skyve.content.BeanContent} — textual bean metadata indexed for
 *       search, without an associated physical file.</li>
 * </ul>
 *
 * <p>The MIME type utilities ({@link org.skyve.content.MimeType},
 * {@link org.skyve.content.Disposition}) for content type mapping and HTTP
 * disposition headers are declared in the {@code skyve-core} {@code org.skyve.content} package.
 *
 * @see org.skyve.content.ContentManager
 * @see org.skyve.content.SearchResults
 */
package org.skyve.content;
