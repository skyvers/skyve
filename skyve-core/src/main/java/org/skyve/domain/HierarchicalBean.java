package org.skyve.domain;

import java.util.List;

/**
 * Extends {@link Bean} for self-referencing tree (hierarchical) documents.
 *
 * <p>Documents declared with a self-referencing parent association in their metadata will
 * have their generated class implement this interface. The tree structure is navigated
 * through a parent ID stored on each node; children are discovered by querying for all
 * beans whose parent ID equals this bean's {@code bizId}.
 *
 * <p>Note that {@link #getParent()} and {@link #getChildren()} are live database queries
 * when called on a non-{@link DynamicHierarchicalBean} instance. Avoid calling them
 * in tight loops or on large trees without a caching strategy.
 *
 * <p>Complexity: {@link #getChildren()} is O(n) where n is the number of direct children
 * in the database.
 *
 * @param <T> the concrete bean type for this hierarchy; typically the document's own type
 * @see Bean
 * @see ChildBean
 */
public interface HierarchicalBean<T extends Bean> extends Bean {
	/**
	 * Binding name for the parent ID property ({@code bizParentId}).
	 * Stores the {@link Bean#DOCUMENT_ID} of this bean's parent node, or {@code null}
	 * if this bean is a root node.
	 */
	public static final String PARENT_ID = "bizParentId";
	
	/**
	 * Returns the {@code bizId} of this bean's parent node.
	 *
	 * @return the parent's ID, or {@code null} if this is a root node
	 */
	public String getBizParentId();
	
	/**
	 * Sets the {@code bizId} of this bean's parent node.
	 *
	 * @param bizParentId the parent ID, or {@code null} to make this a root node
	 */
	public void setBizParentId(String bizParentId);

	/**
	 * Returns the parent bean for this node, or {@code null} if this is a root node.
	 *
	 * <p>The implementation typically issues a database query to load the parent by
	 * {@code bizParentId}. The result is not cached; repeated calls may hit the database.
	 *
	 * @return the parent bean, or {@code null} if this is a root node
	 */
	public T getParent();

	/**
	 * Returns the direct children of this node.
	 *
	 * <p>The implementation queries the database for all beans of this document type
	 * whose {@code bizParentId} equals this bean's {@code bizId}. The list is not
	 * cached; repeated calls will re-query.
	 *
	 * <p>Complexity: O(n) database query where n is the number of direct children.
	 *
	 * @return a mutable list of direct children; never {@code null}, but may be empty
	 */
	public List<T> getChildren();
}
