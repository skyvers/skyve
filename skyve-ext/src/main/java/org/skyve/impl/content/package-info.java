/**
 * Content manager implementations for Skyve's content storage and indexing subsystem.
 *
 * <p>{@code AbstractContentManager} provides the base contract; implementations include
 * {@code FileSystemContentManager} (filesystem-backed) and {@code NoOpContentManager}
 * (discards all operations, useful in headless or test contexts).
 *
 * @see org.skyve.impl.content.ejb
 * @see org.skyve.impl.content.rest
 */
package org.skyve.impl.content;
