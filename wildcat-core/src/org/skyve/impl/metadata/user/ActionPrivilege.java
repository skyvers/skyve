package org.skyve.impl.metadata.user;

import org.skyve.impl.metadata.user.Privilege;

public class ActionPrivilege extends Privilege {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 8111333874339947989L;

	private String documentName;

	public String getDocumentName() {
		return documentName;
	}

	public void setDocumentName(String documentName) {
		this.documentName = documentName;
	}
}
