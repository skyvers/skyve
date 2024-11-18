package org.skyve.domain;

import org.skyve.domain.types.OptimisticLock;

/**
 * 
 */
public interface PersistentBean extends Bean {
	/**
	 * The name of the optimistic lock property.
	 */
	public static final String LOCK_NAME = "bizLock";
	
	/**
	 * The name of the version property.
	 */
	public static final String VERSION_NAME = "bizVersion";
	
	/**
	 * The name of the flag comment property.
	 */
	public static final String FLAG_COMMENT_NAME = "bizFlagComment";
	
	/**
	 * The name of the tagged property.
	 */
	public static final String TAGGED_NAME = "bizTagged";

	/**
	 * The name of the discriminator database column.
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
	 * 
	 * @return
	 */
	public Integer getBizVersion();
	
	/**
	 * 
	 * @param bizVersion
	 */
	public void setBizVersion(Integer bizVersion);

	/**
	 * 
	 * @return
	 */
	public OptimisticLock getBizLock();
	
	/**
	 * 
	 * @param bizLock
	 */
	public void setBizLock(OptimisticLock bizLock);

	/**
	 * 
	 * @param bizKey
	 */
	public void setBizKey(String bizKey);
	
	/**
	 * 
	 * @return
	 */
	public String getBizFlagComment();
	
	/**
	 * 
	 * @param bizFlagComment
	 */
	public void setBizFlagComment(String bizFlagComment);
	
	/**
	 * 
	 * @return
	 */
	public Boolean getBizTagged();

	/**
	 * 
	 * @param bizTagged
	 */
	public void setBizTagged(Boolean bizTagged);
}
