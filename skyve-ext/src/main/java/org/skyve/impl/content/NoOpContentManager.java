package org.skyve.impl.content;

import org.skyve.content.AttachmentContent;
import org.skyve.content.BeanContent;
import org.skyve.content.ContentIterable;
import org.skyve.content.SearchResults;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * No-operation {@link AbstractContentManager} implementation that silently
 * discards all content store operations.
 *
 * <p>Useful as a placeholder in environments where content indexing is not
 * required or is handled externally.
 */
public class NoOpContentManager extends AbstractContentManager {
	@Override
	public void put(@Nonnull BeanContent content) throws Exception {
		// no-op
	}

	@Override
	public void put(@Nonnull AttachmentContent content, boolean index) throws Exception {
		// no-op
	}

	@Override
	public void update(@Nonnull AttachmentContent content) throws Exception {
		// no-op
	}
	
	@Override
	public void reindex(@Nonnull AttachmentContent attachment, boolean index) throws Exception {
		// no-op
	}
	
	@Override
	public @Nullable AttachmentContent getAttachment(@Nonnull String contentId) throws Exception {
		// no-op
		return null;
	}

	@Override
	public void removeBean(@Nonnull String bizId) throws Exception {
		// no-op
	}

	@Override
	public void removeAttachment(@Nonnull String contentId) throws Exception {
		// no-op
	}

	@Override
	public @Nonnull SearchResults google(@Nullable String search, int maxResults) throws Exception {
		// no-op
		return null;
	}

	@Override
	public void dropIndexing() throws Exception {
		// no-op
	}
	
	@Override
	public void truncateIndexing(@Nonnull String customerName) throws Exception {
		// no-op
	}

	@Override
	public void truncateAttachmentIndexing(@Nonnull String customerName) throws Exception {
		// no-op
	}

	@Override
	public void truncateBeanIndexing(@Nonnull String customerName) throws Exception {
		// no-op
	}

	@Override
	public @Nonnull ContentIterable all() throws Exception {
		// no-op
		return null;
	}

	@Override
	public void close() throws Exception {
		// no-op
	}

	@Override
	public void startup() {
		// no-op
	}

	@Override
	public void shutdown() {
		// no-op
	}
}
