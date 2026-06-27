package org.skyve.domain;

import org.skyve.domain.types.OptimisticLock;

/**
 * Extends {@link Bean} with the properties required for database persistence.
 *
 * <p>All documents that persist to the database implement this interface. It adds:
 * <ul>
 *   <li><b>Optimistic locking</b> — {@code bizLock} records the user and timestamp of the
 *       last save; the persistence layer rejects concurrent writes that collide.
 *   <li><b>Version counter</b> — {@code bizVersion} is incremented on every save and used
 *       by Hibernate's optimistic-lock mechanism as a secondary concurrency guard.
 *   <li><b>Flag comment</b> — an operator-facing annotation string visible in admin UI.
 *   <li><b>Tagged</b> — a transient flag set when a bean is included in the currently
 *       active {@link org.skyve.EXT#getTagManager() tag} set.
 * </ul>
 *
 * <p>The {@code biz*} constants here are the canonical binding names used in queries,
 * EL expressions, and view bindings. Prefer these constants over string literals.
 *
 * @see Bean
 * @see org.skyve.domain.types.OptimisticLock
 * @see org.skyve.persistence.Persistence
 */
public interface PersistentBean extends Bean {
	/**
	 * Binding name for the optimistic lock property ({@code bizLock}).
	 * Stores the user name and UTC timestamp of the last successful save.
	 */
	public static final String LOCK_NAME = "bizLock";
	
	/**
	 * Binding name for the Hibernate version counter ({@code bizVersion}).
	 * Incremented by the persistence layer on every save to detect concurrent modifications.
	 */
	public static final String VERSION_NAME = "bizVersion";
	
	/**
	 * Binding name for the flag comment property ({@code bizFlagComment}).
	 * An optional free-text note an operator can attach to any persistent bean via the
	 * admin flag UI. Not used by the framework for any logic.
	 */
	public static final String FLAG_COMMENT_NAME = "bizFlagComment";
	
	/**
	 * Binding name for the tagged property ({@code bizTagged}).
	 * Set transiently to {@code true} when this bean is a member of the current tag set.
	 */
	public static final String TAGGED_NAME = "bizTagged";

	/**
	 * Name of the discriminator database column used in single-table and joined-table
	 * inheritance strategies to identify the concrete subtype.
	 */
	public static final String DISCRIMINATOR_NAME = "bizDiscriminator";

	/**
	 * The name of the owner ID database column in a joining table.
	 */
	public static final String OWNER_COLUMN_NAME = "owner_id";
	
	/**
	 * The name of the element ID database column in a joining table.
	 */
	public static final String ELEMENT_COLUMN_NAME = "element_id";
	
	/**
	 * Returns the Hibernate version counter for this bean.
	 *
	 * @return the current version, or {@code null} if the bean has never been persisted
	 */
	public Integer getBizVersion();
	
	/**
	 * Sets the Hibernate version counter. Normally only called by the persistence layer.
	 *
	 * @param bizVersion the version value
	 */
	public void setBizVersion(Integer bizVersion);

	/**
	 * Returns the optimistic lock for this bean, recording who last saved it and when.
	 *
	 * <p>The persistence layer checks this value on save and throws a
	 * {@link org.skyve.domain.messages.OptimisticLockException} if a concurrent
	 * modification is detected.
	 *
	 * @return the current optimistic lock, or {@code null} if never persisted
	 */
	public OptimisticLock getBizLock();
	
	/**
	 * Sets the optimistic lock. Normally only called by the persistence layer after a
	 * successful save.
	 *
	 * @param bizLock the new lock value
	 */
	public void setBizLock(OptimisticLock bizLock);

	/**
	 * Sets the human-readable display key for this persistent bean.
	 *
	 * <p>The persistence layer calls this after every save to keep the stored
	 * {@code bizKey} column in sync with the document's key expression.
	 *
	 * @param bizKey the display key; may be {@code null}
	 */
	public void setBizKey(String bizKey);
	
	/**
	 * Returns the operator flag comment attached to this bean.
	 *
	 * @return the flag comment, or {@code null} if none has been set
	 */
	public String getBizFlagComment();
	
	/**
	 * Sets the operator flag comment on this bean.
	 *
	 * @param bizFlagComment the comment text, or {@code null} to clear the flag
	 */
	public void setBizFlagComment(String bizFlagComment);
	
	/**
	 * Returns whether this bean is currently included in the active tag set.
	 *
	 * <p>This property is populated transiently by list queries when a tag is active;
	 * it is not persisted to the database.
	 *
	 * @return {@code Boolean.TRUE} if tagged, {@code Boolean.FALSE} or {@code null} otherwise
	 */
	public Boolean getBizTagged();

	/**
	 * Sets the transient tagged flag. Called by the framework during list query population
	 * when a tag is active; not normally called from application code.
	 *
	 * @param bizTagged {@code Boolean.TRUE} if this bean should be marked as tagged
	 */
	public void setBizTagged(Boolean bizTagged);
}
