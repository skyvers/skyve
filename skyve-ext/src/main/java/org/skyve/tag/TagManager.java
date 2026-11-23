package org.skyve.tag;

import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.model.document.ModuleDocument;
import org.skyve.persistence.AutoClosingIterable;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Manages persistent user-defined tags applied to beans.
 *
 * <p>Tags are named groupings (owned by admin Tag records) that associate arbitrary
 * beans (identified by module name, document name, and bizId). A single bean can
 * carry any number of tags, and a tag can reference beans from any document type.
 *
 * <p>Typical usage patterns:
 * <ul>
 *   <li>Bulk-select list rows into a temporary tag, then operate on all tagged beans.</li>
 *   <li>Persistent user bookmarks: tag individual records from multiple lists.</li>
 *   <li>Set operations ({@link #intersect}, {@link #union}, {@link #subtract}) between
 *       two tags to produce a third tag for pipeline workflows.</li>
 * </ul>
 *
 * <p>Obtain a {@code TagManager} via {@link org.skyve.EXT#getTagManager()}.
 *
 * <p>Threading: a {@code TagManager} instance is not thread-safe. Do not share across
 * concurrent threads.
 */
public interface TagManager {
	/**
	 * Tag 1 bean.
	 * 
	 * @param tagId
	 *            Tag the bean against this tag.
	 * @param bean
	 *            The bean to be tagged.
	 * @throws Exception
	 */
	void tag(@Nonnull String tagId, @Nonnull Bean bean) throws Exception;

	/**
	 * Tag 1 bean.
	 * 
	 * @param tagId
	 *            Tag the bean against this tag.
	 * @param taggedModuleName
	 *            The module name of the bean to be tagged.
	 * @param taggedDocumentName
	 *            The document name of the bean to be tagged.
	 * @param taggedBizId
	 *            The bizId of the bean to be tagged.
	 * @throws Exception
	 */
	void tag(@Nonnull String tagId,
				@Nonnull String taggedModuleName,
				@Nonnull String taggedDocumentName,
				@Nonnull String taggedBizId)
	throws Exception;

	/**
	 * Tag 1 bean; a typed shortcut for {@link #tag(String, String, String, String)}.
	 * Pass the constant of the document's owning module, such as {@code AdminDocument.CONTACT}:
	 * the names are stored as given, and {@link #untag(String, Bean)} matches the bean's own module.
	 * 
	 * @param tagId
	 *            Tag the bean against this tag.
	 * @param document
	 *            The compile-time document reference of the bean to be tagged.
	 * @param taggedBizId
	 *            The bizId of the bean to be tagged.
	 * @throws Exception
	 */
	default void tag(@Nonnull String tagId,
						@Nonnull ModuleDocument document,
						@Nonnull String taggedBizId)
	throws Exception {
		tag(tagId, document.moduleName(), document.documentName(), taggedBizId);
	}

	/**
	 * Remove the bean from this tag.
	 * 
	 * @param tagId
	 *            The tag.
	 * @param bean
	 *            The bean to untag.
	 * @throws Exception
	 */
	void untag(@Nonnull String tagId, @Nonnull Bean bean) throws Exception;

	/**
	 * Remove the bean from this tag.
	 * 
	 * @param tagId
	 *            The tag.
	 * @param taggedModuleName
	 *            The module name of the bean to be untagged.
	 * @param taggedDocumentName
	 *            The document name of the bean to be untagged.
	 * @param taggedBizId
	 *            The bizId of the bean to be untagged.
	 * @throws Exception
	 */
	void untag(@Nonnull String tagId,
				@Nonnull String taggedModuleName,
				@Nonnull String taggedDocumentName,
				@Nonnull String taggedBizId)
	throws Exception;

	/**
	 * Remove the bean from this tag; a typed shortcut for {@link #untag(String, String, String, String)}.
	 * Pass the constant of the document's owning module, such as {@code AdminDocument.CONTACT}:
	 * the names are matched as given against those stored when the bean was tagged.
	 * 
	 * @param tagId
	 *            The tag.
	 * @param document
	 *            The compile-time document reference of the bean to be untagged.
	 * @param taggedBizId
	 *            The bizId of the bean to be untagged.
	 * @throws Exception
	 */
	default void untag(@Nonnull String tagId,
						@Nonnull ModuleDocument document,
						@Nonnull String taggedBizId)
	throws Exception {
		untag(tagId, document.moduleName(), document.documentName(), taggedBizId);
	}

	/**
	 * Create a tag.
	 * 
	 * @param tagName
	 *            The name of the tag.
	 * @param visible
	 *            Whether the tag should be shown throughout the skyve UI.
	 * @return The tagId of the created tag.
	 * @throws Exception
	 */
	@Nonnull String create(@Nonnull String tagName, boolean visible) throws Exception;

	/**
	 * Retrieve the tagId of the named tag.
	 * 
	 * @param tagName
	 *            The name of the tag to retrieve.
	 * @return The corresponding tagId
	 * @throws Exception
	 */
	@Nullable String getTagId(@Nonnull String tagName) throws Exception;

	/**
	 * Returns all available tags as domain values for UI selection controls.
	 *
	 * @return Tag values where code is tag ID and description is display name
	 * @throws Exception If tag retrieval fails
	 */
	@Nonnull List<DomainValue> getTags() throws Exception;

	/**
	 * Delete a tag.
	 * 
	 * @param tagId
	 *            The tagId of the tag to delete.
	 * @throws Exception
	 */
	void delete(@Nonnull String tagId) throws Exception;

	/**
	 * Tag a bunch of beans.
	 * 
	 * @param tagId
	 *            The tag to use.
	 * @param beans
	 *            The beans to tag.
	 * @throws Exception
	 */
	void tag(@Nonnull String tagId, @Nonnull Iterable<Bean> beans) throws Exception;

	/**
	 * Untag (remove) a bunch of beans.
	 * 
	 * @param tagId
	 *            The tag to remove from.
	 * @param beans
	 *            The beans to untag.
	 * @throws Exception
	 */
	void untag(@Nonnull String tagId, @Nonnull Iterable<Bean> beans) throws Exception;

	/**
	 * Clear any beans related to the given tag.
	 * 
	 * @param tagId
	 *            the given tag
	 * @throws Exception
	 */
	void clear(@Nonnull String tagId) throws Exception;

	/**
	 * Iterate over the tagged beans.
	 * 
	 * @param tagId
	 *            The tag to iterate.
	 * @return The beans (a scrolled set). Each bean is loaded into 1st level
	 *         cache so beware.
	 * @throws Exception
	 */
	@Nonnull AutoClosingIterable<Bean> iterate(@Nonnull String tagId) throws Exception;
}
