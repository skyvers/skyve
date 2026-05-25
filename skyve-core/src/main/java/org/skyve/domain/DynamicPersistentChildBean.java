package org.skyve.domain;

import java.util.Map;

/**
 * A dynamic (schema-less) {@link PersistentBean} that is also a {@link ChildBean}.
 *
 * <p>Used for dynamic documents that are persisted and owned as children within a
 * parent collection. Combines the capabilities of {@link DynamicPersistentBean} and
 * {@link ChildBean} with the child's parent reference and ordinal stored in the
 * underlying dynamic map.
 *
 * @see DynamicPersistentBean
 * @see ChildBean
 */
public class DynamicPersistentChildBean extends DynamicPersistentBean implements ChildBean<Bean> {
	private static final long serialVersionUID = -1007733210670332962L;

	/**
	 * Creates a new DynamicPersistentChildBean instance.
	 * @param bizModule the bizModule
	 * @param bizDocument the bizDocument
	 * @param properties the properties
	 */
	public DynamicPersistentChildBean(String bizModule, String bizDocument, Map<String, Object> properties) {
		super(bizModule, bizDocument, properties);
	}

	/**
	 * Returns the parent.
	 * @return the result
	 */
	@Override
	public Bean getParent() {
		return (Bean) get(ChildBean.PARENT_NAME);
	}

	/**
	 * Sets the parent.
	 * @param parent the parent
	 */
	@Override
	public void setParent(Bean parent) {
		set(ChildBean.PARENT_NAME, parent);
	}

	/**
	 * Returns the bizOrdinal.
	 * @return the result
	 */
	@Override
	public Integer getBizOrdinal() {
		return (Integer) get(Bean.ORDINAL_NAME);
	}

	/**
	 * Sets the bizOrdinal.
	 * @param bizOrdinal the bizOrdinal
	 */
	@Override
	public void setBizOrdinal(Integer bizOrdinal) {
		set(Bean.ORDINAL_NAME, bizOrdinal);
	}
}
