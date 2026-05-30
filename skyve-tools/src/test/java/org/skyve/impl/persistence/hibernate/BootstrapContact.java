package org.skyve.impl.persistence.hibernate;

import java.util.LinkedHashSet;
import java.util.Set;

import org.skyve.domain.app.AppConstants;
import org.skyve.impl.domain.AbstractPersistentBean;

public class BootstrapContact extends AbstractPersistentBean {
	private static final long serialVersionUID = -5057676849428559548L;

	private String bizKey;
	@SuppressWarnings("unused")
	private Set<String> roles = new LinkedHashSet<>();

	@Override
	public String getBizModule() {
		return AppConstants.ADMIN_MODULE_NAME;
	}

	@Override
	public String getBizDocument() {
		return AppConstants.CONTACT_DOCUMENT_NAME;
	}

	@Override
	public String getBizKey() {
		return bizKey;
	}

	@Override
	public void setBizKey(String bizKey) {
		super.setBizKey(bizKey);
		this.bizKey = bizKey;
	}
}