package org.skyve.domain;

import java.util.Map;

/**
 * A dynamic (schema-less) {@link Bean} that also implements {@link ChildBean}.
 *
 * <p>Used for dynamic documents that are owned children within a parent bean's collection.
 * The parent reference and ordinal are stored in the underlying dynamic map under the
 * standard {@link ChildBean#PARENT_NAME} and {@link Bean#ORDINAL_NAME} keys.
 *
 * @see DynamicBean
 * @see ChildBean
 */
public class DynamicChildBean extends DynamicBean implements ChildBean<Bean> {
	private static final long serialVersionUID = 7086283769532783686L;

	/**
	 * Creates a new DynamicChildBean instance.
	 * @param bizModule the bizModule
	 * @param bizDocument the bizDocument
	 * @param properties the properties
	 */
	public DynamicChildBean(String bizModule, String bizDocument, Map<String, Object> properties) {
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
