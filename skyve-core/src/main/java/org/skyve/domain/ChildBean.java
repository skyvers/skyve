package org.skyve.domain;

/**
 * Extends {@link Bean} for documents that are owned as children within a parent bean's collection.
 *
 * <p>When a document is declared as a collection member of another document with composition
 * (owned) semantics, its generated class will implement this interface. The child maintains a
 * typed back-reference to its parent and an ordinal that tracks its position within the
 * parent's list for display and sort purposes.
 *
 * <p>The parent reference is eagerly loaded and always available without a database query
 * once the child bean has been loaded as part of its parent's collection.
 *
 * <p>Child beans are typically not queried or saved independently; they are loaded and saved
 * as part of their parent bean's object graph through the parent's
 * {@link org.skyve.persistence.Persistence} operations.
 *
 * @param <T> the concrete type of the parent bean
 * @see Bean
 * @see HierarchicalBean
 */
public interface ChildBean<T extends Bean> extends Bean {
	/**
	 * Binding name for the parent reference ({@code parent}).
	 * Used in EL expressions and queries to navigate from a child to its parent bean.
	 */
	public static final String PARENT_NAME = "parent";
	
	/**
	 * Binding suffix for navigating to the parent from a child collection binding
	 * (e.g. {@code "myCollection.parent"}).
	 */
	public static final String CHILD_PARENT_NAME_SUFFIX = "." + ChildBean.PARENT_NAME;
	
	/**
	 * Name of the foreign-key column in the child's database table that points to the
	 * parent's primary key ({@code parent_id}).
	 */
	public static final String CHILD_PARENT_ID = PARENT_NAME + "_id";

	/**
	 * Returns the parent bean that owns this child.
	 *
	 * @return the parent; never {@code null} for a properly loaded child bean
	 */
	public T getParent();
	
	/**
	 * Sets the parent bean that owns this child.
	 *
	 * <p>Normally called by the framework when the child is added to the parent's
	 * collection. Calling this from application code is only necessary when manually
	 * constructing or reparenting a child bean.
	 *
	 * @param parent the owning parent bean; must not be {@code null}
	 */
	public void setParent(T parent);
	
	/**
	 * Returns the zero-based ordinal position of this child within the parent's collection.
	 *
	 * <p>Ordinals are maintained by the framework when items are added, removed, or
	 * reordered in a collection. They determine the default display order in list and
	 * grid widgets.
	 *
	 * @return the ordinal, or {@code null} for a newly created child that has not yet
	 *         been added to a parent collection
	 */
	public Integer getBizOrdinal();
	
	/**
	 * Sets the ordinal position of this child within the parent's collection.
	 *
	 * <p>Normally managed by the framework; application code should not set this
	 * directly except when programmatically reordering collections.
	 *
	 * @param bizOrdinal the new zero-based ordinal position
	 */
	public void setBizOrdinal(Integer bizOrdinal);
}
