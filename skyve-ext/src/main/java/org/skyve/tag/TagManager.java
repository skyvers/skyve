package org.skyve.tag;

import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.metadata.model.document.Bizlet.DomainValue;

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
	void tag(String tagId, Bean bean) throws Exception;

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
	void tag(String tagId,
				String taggedModuleName,
				String taggedDocumentName,
				String taggedBizId)
	throws Exception;

	/**
	 * Remove the bean from this tag.
	 * 
	 * @param tagId
	 *            The tag.
	 * @param bean
	 *            The bean to untag.
	 * @throws Exception
	 */
	void untag(String tagId, Bean bean) throws Exception;

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
	void untag(String tagId,
				String taggedModuleName,
				String taggedDocumentName,
				String taggedBizId)
	throws Exception;

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
	String create(String tagName, boolean visible) throws Exception;

	/**
	 * Retrieve the tagId of the named tag.
	 * 
	 * @param tagName
	 *            The name of the tag to retrieve.
	 * @return The corresponding tagId
	 * @throws Exception
	 */
	String getTagId(String tagName) throws Exception;

	/**
	 * 
	 * @return
	 * @throws Exception
	 */
	List<DomainValue> getTags() throws Exception;

	/**
	 * Delete a tag.
	 * 
	 * @param tagId
	 *            The tagId of the tag to delete.
	 * @throws Exception
	 */
	void delete(String tagId) throws Exception;

	/**
	 * Tag a bunch of beans.
	 * 
	 * @param tagId
	 *            The tag to use.
	 * @param beans
	 *            The beans to tag.
	 * @throws Exception
	 */
	void tag(String tagId, Iterable<Bean> beans) throws Exception;

	/**
	 * Untag (remove) a bunch of beans.
	 * 
	 * @param tagId
	 *            The tag to remove from.
	 * @param beans
	 *            The beans to untag.
	 * @throws Exception
	 */
	void untag(String tagId, Iterable<Bean> beans) throws Exception;

	/**
	 * Clear any beans related to the given tag.
	 * 
	 * @param tagId
	 *            the given tag
	 * @throws Exception
	 */
	void clear(String tagId) throws Exception;

	/**
	 * Iterate over the tagged beans.
	 * 
	 * @param tagId
	 *            The tag to iterate.
	 * @return The beans (a scrolled set). Each bean is loaded into 1st level
	 *         cache so beware.
	 * @throws Exception
	 */
	Iterable<Bean> iterate(String tagId) throws Exception;
}
