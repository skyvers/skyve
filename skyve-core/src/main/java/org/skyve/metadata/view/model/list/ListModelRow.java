package org.skyve.metadata.view.model.list;

import java.util.Map;

import org.skyve.domain.Bean;
import org.skyve.domain.MapBean;
import org.skyve.domain.PersistentBean;

public class ListModelRow extends MapBean {
	private static final long serialVersionUID = 477773812246146903L;
/*
	public ListModelRow(MapBean bean) {
		super(bean.getBizModule(), bean.getBizDocument(), bean.getValues());
	}
	
	private static Map<String, Object> getValues(MapBean bean) {
		Map<String, Object>
		for (DynaProperty property : bean.getDynaProperties()) {
			String propertyName = property.getName();
			result.append(propertyName).append(" = ");
			result.append(get(propertyName)).append(' ');
		}
	}
*/
	public ListModelRow(String bizModule, 
							String bizDocument,
							Map<String, Object> properties) {
		super(bizModule, bizDocument, properties);
	}
	
	private String bizLock;
	public String getBizLock() {
		if (isDynaProperty(PersistentBean.LOCK_NAME) || isDynaProperty(BEAN_PROPERTY_KEY)) {
			return (String) get(PersistentBean.LOCK_NAME);
		}
		return bizLock;
	}
	public void setBizLock(String bizLock) {
		if (isDynaProperty(PersistentBean.LOCK_NAME) || isDynaProperty(BEAN_PROPERTY_KEY)) {
			set(PersistentBean.LOCK_NAME, bizLock);
		}
		else {
			this.bizLock = bizLock;
		}
	}
	
	private String bizTagged;
	public String getBizTagged() {
		if (isDynaProperty(PersistentBean.TAGGED_NAME) || isDynaProperty(BEAN_PROPERTY_KEY)) {
			return (String) get(PersistentBean.TAGGED_NAME);
		}
		return bizTagged;
	}
	public void setBizTagged(String bizTagged) {
		if (isDynaProperty(PersistentBean.TAGGED_NAME) || isDynaProperty(BEAN_PROPERTY_KEY)) {
			set(PersistentBean.TAGGED_NAME, bizLock);
		}
		else {
			this.bizTagged = bizTagged;
		}
	}
	
	private String bizFlagComment;
	public String getBizFlagComment() {
		if (isDynaProperty(PersistentBean.FLAG_COMMENT_NAME) || isDynaProperty(BEAN_PROPERTY_KEY)) {
			return (String) get(PersistentBean.FLAG_COMMENT_NAME);
		}
		return bizFlagComment;
	}
	public void setBizFlagComment(String bizFlagComment) {
		if (isDynaProperty(PersistentBean.FLAG_COMMENT_NAME) || isDynaProperty(BEAN_PROPERTY_KEY)) {
			set(PersistentBean.FLAG_COMMENT_NAME, bizLock);
		}
		else {
			this.bizFlagComment = bizFlagComment;
		}
	}

	private String bizKey;
	@Override
	public String getBizKey() {
		if (isDynaProperty(Bean.BIZ_KEY) || isDynaProperty(BEAN_PROPERTY_KEY)) {
			return (String) get(Bean.BIZ_KEY);
		}
		return bizKey;
	}
	public void setBizKey(String bizKey) {
		if (isDynaProperty(Bean.BIZ_KEY) || isDynaProperty(BEAN_PROPERTY_KEY)) {
			set(Bean.BIZ_KEY, bizLock);
		}
		else {
			this.bizKey = bizKey;
		}
	}
}
