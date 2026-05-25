package org.skyve.domain;

import java.util.Map;

import org.skyve.domain.types.OptimisticLock;
import org.skyve.impl.persistence.AbstractPersistence;

/**
 * A dynamic (schema-less) {@link Bean} that also implements {@link PersistentBean}.
 *
 * <p>Used for dynamic documents that are persisted to the database. The standard
 * persistence fields ({@code bizLock}, {@code bizVersion}, {@code bizFlagComment},
 * {@code bizTagged}) are stored in the underlying dynamic map under their canonical
 * binding names.
 *
 * <p>{@link #isPersisted()} delegates to the current thread's
 * {@link org.skyve.persistence.Persistence} identity map, so it reflects the actual
 * database state for the current session.
 *
 * @see DynamicBean
 * @see PersistentBean
 */
public class DynamicPersistentBean extends DynamicBean implements PersistentBean {
	private static final long serialVersionUID = -689278030377120098L;

	/**
	 * Creates a new DynamicPersistentBean instance.
	 * @param bizModule the bizModule
	 * @param bizDocument the bizDocument
	 * @param properties the properties
	 */
	public DynamicPersistentBean(String bizModule, String bizDocument, Map<String, Object> properties) {
		super(bizModule, bizDocument, properties);
	}

	/**
	 * Returns the bizVersion.
	 * @return the result
	 */
	@Override
	public Integer getBizVersion() {
		return (Integer) get(PersistentBean.VERSION_NAME);
	}

	/**
	 * Sets the bizVersion.
	 * @param bizVersion the bizVersion
	 */
	@Override
	public void setBizVersion(Integer bizVersion) {
		set(PersistentBean.VERSION_NAME, bizVersion);
	}

	/**
	 * Returns the bizLock.
	 * @return the result
	 */
	@Override
	public OptimisticLock getBizLock() {
		return (OptimisticLock) get(PersistentBean.LOCK_NAME);
	}

	/**
	 * Sets the bizLock.
	 * @param bizLock the bizLock
	 */
	@Override
	public void setBizLock(OptimisticLock bizLock) {
		set(PersistentBean.LOCK_NAME, bizLock);
	}

	/**
	 * Sets the bizKey.
	 * @param bizKey the bizKey
	 */
	@Override
	public void setBizKey(String bizKey) {
		set(Bean.BIZ_KEY, bizKey);
	}

	/**
	 * Returns the bizFlagComment.
	 * @return the result
	 */
	@Override
	public String getBizFlagComment() {
		return (String) get(PersistentBean.FLAG_COMMENT_NAME);
	}

	/**
	 * Sets the bizFlagComment.
	 * @param bizFlagComment the bizFlagComment
	 */
	@Override
	public void setBizFlagComment(String bizFlagComment) {
		set(PersistentBean.FLAG_COMMENT_NAME, bizFlagComment);
	}

	/**
	 * Returns the bizTagged.
	 * @return the result
	 */
	@Override
	public Boolean getBizTagged() {
		return (Boolean) get(PersistentBean.TAGGED_NAME);
	}

	/**
	 * Sets the bizTagged.
	 * @param bizTagged the bizTagged
	 */
	@Override
	public void setBizTagged(Boolean bizTagged) {
		set(PersistentBean.TAGGED_NAME, bizTagged);
	}
	
	/**
	 * Returns whether persisted.
	 * @return the result
	 */
	@Override
	public boolean isPersisted() {
		return AbstractPersistence.get().isPersisted(this);
	}

	/**
	 * Returns whether notPersisted.
	 * @return the result
	 */
	@Override
	public boolean isNotPersisted() {
		return (! AbstractPersistence.get().isPersisted(this));
	}
}
