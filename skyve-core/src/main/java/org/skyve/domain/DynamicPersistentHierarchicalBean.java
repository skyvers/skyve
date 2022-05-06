package org.skyve.domain;

import java.util.List;
import java.util.Map;

import org.skyve.CORE;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;

public class DynamicPersistentHierarchicalBean extends DynamicPersistentBean implements HierarchicalBean<Bean> {
	private static final long serialVersionUID = 9192062244299454210L;

	public DynamicPersistentHierarchicalBean(String bizModule, String bizDocument, Map<String, Object> properties) {
		super(bizModule, bizDocument, properties);
	}

	@Override
	public String getBizParentId() {
		return (String) get(HierarchicalBean.PARENT_ID);
	}

	@Override
	public void setBizParentId(String bizParentId) {
		set(HierarchicalBean.PARENT_ID, bizParentId);
	}

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

	@Override
	public List<Bean> getChildren() {
		Persistence p = CORE.getPersistence();
		DocumentQuery q = p.newDocumentQuery(getBizModule(), getBizDocument());
		q.getFilter().addEquals(HierarchicalBean.PARENT_ID, getBizId());
		return q.beanResults();
	}
}
