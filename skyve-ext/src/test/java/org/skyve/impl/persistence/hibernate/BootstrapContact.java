package org.skyve.impl.persistence.hibernate;

import java.util.LinkedHashSet;
import java.util.Set;

import org.skyve.domain.app.AppConstants;
import org.skyve.impl.domain.AbstractPersistentBean;

/**
 * Test-only minimal persistent bean used by
 * {@link AbstractHibernatePersistenceTest}.
 * <p>
 * It exists solely to provide a lightweight `adminContact` Hibernate mapping so
 * {@link AbstractHibernatePersistence} can initialise in unit tests without the
 * full generated domain model.
 * </p>
 */
public class BootstrapContact extends AbstractPersistentBean {
	private static final long serialVersionUID = -6815798403338244649L;

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
