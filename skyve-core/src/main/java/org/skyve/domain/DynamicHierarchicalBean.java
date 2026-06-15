package org.skyve.domain;

import java.util.List;
import java.util.Map;

import org.skyve.CORE;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;

/**
 * A dynamic (schema-less) {@link Bean} that also implements {@link HierarchicalBean}.
 *
 * <p>Used for dynamic documents that form self-referencing tree structures.
 * {@link #getParent()} and {@link #getChildren()} issue live database queries via
 * the current thread's {@link org.skyve.persistence.Persistence}.
 *
 * @see DynamicBean
 * @see HierarchicalBean
 */
public class DynamicHierarchicalBean extends DynamicBean implements HierarchicalBean<Bean> {
	private static final long serialVersionUID = -7668370665710632246L;

	/**
	 * Creates a new DynamicHierarchicalBean instance.
	 * @param bizModule the bizModule
	 * @param bizDocument the bizDocument
	 * @param properties the properties
	 */
	public DynamicHierarchicalBean(String bizModule, String bizDocument, Map<String, Object> properties) {
		super(bizModule, bizDocument, properties);
	}

	/**
	 * Returns the bizParentId.
	 * @return the result
	 */
	@Override
	public String getBizParentId() {
		return (String) get(HierarchicalBean.PARENT_ID);
	}

	/**
	 * Sets the bizParentId.
	 * @param bizParentId the bizParentId
	 */
	@Override
	public void setBizParentId(String bizParentId) {
		set(HierarchicalBean.PARENT_ID, bizParentId);
	}

	/**
	 * Returns the parent.
	 * @return the result
	 */
	@Override
	public Bean getParent() {
		Bean result = null;
		
		final String bizParentId = getBizParentId();
		if (bizParentId != null) {
			Persistence p = CORE.getPersistence();
			DocumentQuery q = p.newDocumentQuery(getBizModule(), getBizDocument());
			q.getFilter().addEquals(Bean.DOCUMENT_ID, bizParentId);
			result = q.retrieveBean();
		}
		
		return result;
	}

	/**
	 * Returns the children.
	 * @return the result
	 */
	@Override
	public List<Bean> getChildren() {
		Persistence p = CORE.getPersistence();
		DocumentQuery q = p.newDocumentQuery(getBizModule(), getBizDocument());
		q.getFilter().addEquals(HierarchicalBean.PARENT_ID, getBizId());
		return q.beanResults();
	}
}
