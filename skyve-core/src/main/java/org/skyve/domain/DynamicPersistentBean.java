package org.skyve.domain;

import java.util.Map;

import org.skyve.domain.types.OptimisticLock;
import org.skyve.impl.persistence.AbstractPersistence;

public class DynamicPersistentBean extends DynamicBean implements PersistentBean {
	private static final long serialVersionUID = -689278030377120098L;

	public DynamicPersistentBean(String bizModule, String bizDocument, Map<String, Object> properties) {
		super(bizModule, bizDocument, properties);
	}

	@Override
	public Integer getBizVersion() {
		return (Integer) get(PersistentBean.VERSION_NAME);
	}

	@Override
	public void setBizVersion(Integer bizVersion) {
		set(PersistentBean.VERSION_NAME, bizVersion);
	}

	@Override
	public OptimisticLock getBizLock() {
		return (OptimisticLock) get(PersistentBean.LOCK_NAME);
	}

	@Override
	public void setBizLock(OptimisticLock bizLock) {
		set(PersistentBean.LOCK_NAME, bizLock);
	}

	@Override
	public void setBizKey(String bizKey) {
		set(Bean.BIZ_KEY, bizKey);
	}

	@Override
	public String getBizFlagComment() {
		return (String) get(PersistentBean.FLAG_COMMENT_NAME);
	}

	@Override
	public void setBizFlagComment(String bizFlagComment) {
		set(PersistentBean.FLAG_COMMENT_NAME, bizFlagComment);
	}

	@Override
	public Boolean getBizTagged() {
		return (Boolean) get(PersistentBean.TAGGED_NAME);
	}

	@Override
	public void setBizTagged(Boolean bizTagged) {
		set(PersistentBean.TAGGED_NAME, bizTagged);
	}
	
	@Override
	public boolean isPersisted() {
		return AbstractPersistence.get().isPersisted(this);
	}

	@Override
	public boolean isNotPersisted() {
		return (! AbstractPersistence.get().isPersisted(this));
	}
}
