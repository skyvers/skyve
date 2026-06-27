package org.skyve.impl.metadata.user;

/**
 * A privilege granting the right to invoke a named action on a specific document
 * within a role.
 *
 * <p>Pairs a {@code documentName} with the inherited {@code name} (the action
 * class name) from {@link Privilege}.
 */
public class ActionPrivilege extends Privilege {
	private static final long serialVersionUID = 8111333874339947989L;

	private String documentName;

	public String getDocumentName() {
		return documentName;
	}

	public void setDocumentName(String documentName) {
		this.documentName = documentName;
	}
}
